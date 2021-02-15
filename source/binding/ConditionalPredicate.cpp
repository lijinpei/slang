#include "slang/binding/ConditionalPredicate.h"

#include <memory>

#include "slang/binding/BindContext.h"
#include "slang/binding/EvalContext.h"
#include "slang/binding/Expression.h"
#include "slang/compilation/Compilation.h"
#include "slang/diagnostics/ExpressionsDiags.h"
#include "slang/types/AllTypes.h"
#include "slang/types/Type.h"
#include "slang/symbols/VariableSymbols.h"
#include "slang/syntax/AllSyntax.h"

namespace slang {
MatchedPattern::MatchedPattern(PatternKind kind, const PatternSyntax* syntax) :
    kind(kind), syntax(syntax),
    location(syntax ? syntax->sourceRange().start() : SourceLocation::NoLocation) {}

void MatchedPattern::resolve(const BindContext& context, const Type& type) {
    switch (kind) {
        default:
            THROW_UNREACHABLE;
#define CASE(x) \
    case PatternKind::x: \
        return this->as<x ## Pattern>().resolve(context, type)
        CASE(Wildcard);
        CASE(Variable);
        CASE(Expression);
        CASE(Tagged);
        CASE(OrderedStructure);
        CASE(NamedStructure);
#undef CASE
    }
}

MatchedPattern& MatchedPattern::preBind(Compilation& comp, const PatternSyntax& syntax, const Scope& scope) {
    switch (syntax.kind) {
        default:
            THROW_UNREACHABLE;
#define CASE(x) \
        case SyntaxKind::x: \
            return x::preBind(comp, syntax.as<x ## Syntax>(), scope);
#define CASE_NO_SCOPE(x) \
        case SyntaxKind::x: \
            return x::preBind(comp, syntax.as<x ## Syntax>());
        CASE(VariablePattern)
        CASE_NO_SCOPE(WildcardPattern)
        CASE_NO_SCOPE(ExpressionPattern)
        case SyntaxKind::ParenthesizedPattern:
            return MatchedPattern::preBind(comp, stripAwayParentheses(syntax.as<ParenthesizedPatternSyntax>()), scope);
        CASE(TaggedPattern)
        CASE(OrderedStructurePattern)
        CASE(NamedStructurePattern)
#undef CASE
#undef CASE_NO_SCOPE
    }
}

const PatternSyntax& MatchedPattern::stripAwayParentheses(const ParenthesizedPatternSyntax& syntax_) {
    const ParenthesizedPatternSyntax* syntax = &syntax_;
    while (ParenthesizedPatternSyntax::isKind(syntax->pattern->kind)) {
        syntax = &syntax->pattern->as<ParenthesizedPatternSyntax>();
    }
    return *syntax->pattern;
}

bool MatchedPattern::bad() const {
    // FIXME: will a pattern be bad?
    return false;
}

WildcardPattern::WildcardPattern(const WildcardPatternSyntax* syntax) :
        MatchedPattern(PatternKind::Wildcard, syntax) {}

WildcardPattern& WildcardPattern::preBind(Compilation& comp, const WildcardPatternSyntax& syntax) {
    return *comp.emplace<WildcardPattern>(&syntax);
}

void WildcardPattern::resolve(const BindContext&, const Type& type) {
    this->type = &type;
}

VariablePattern::VariablePattern(const VariablePatternSyntax* syntax, PatternBindingSymbol& symbol) :
        MatchedPattern(PatternKind::Variable, syntax), symbol(symbol) {}

VariablePattern& VariablePattern::preBind(Compilation& comp, const VariablePatternSyntax& syntax, const Scope& parentScope) {
    auto& symbol = *comp.emplace<PatternBindingSymbol>(syntax.variableName.valueText(), syntax.sourceRange().start());
    parentScope.addMember(symbol);
    auto& result = *comp.emplace<VariablePattern>(&syntax, symbol);
    return result;
}

void VariablePattern::resolve(const BindContext&, const Type& type) {
    this->type = &type;
    symbol.setType(type);
}

ExpressionPattern::ExpressionPattern(const ExpressionPatternSyntax* syntax) :
        MatchedPattern(PatternKind::Expression, syntax) {}

ExpressionPattern& ExpressionPattern::preBind(Compilation& comp, const ExpressionPatternSyntax& syntax) {
    return *comp.emplace<ExpressionPattern>(&syntax);
}

void ExpressionPattern::resolve(const BindContext& context, const Type& type_) {
    this->type = &type_;
    // FIXME: is BindFlags::Constant necessary?
    expr = &Expression::bind(*syntax->as<ExpressionPatternSyntax>().expr, context, BindFlags::Constant);
    // FIXME: should set EvalFlags?
    EvalContext evalCtx(context.scope.getCompilation()) ;
    auto cv = expr->eval(evalCtx);
    if (!cv) {
        context.addDiag(diag::ExpressionPatternNotConstant, syntax->sourceRange());
        return;
    }
    // FIXME: better type compare?
    const auto& type = type_.getCanonicalType();
    if (!type.isIntegral()) {
        context.addDiag(diag::ExpressionPatternNonIntegral, syntax->sourceRange())
            .addNote(diag::PatternContextType, syntax->sourceRange().start()) << type_;
        return;
    }
    const auto& exprType = expr->type->getCanonicalType();
    if (!type.isMatching(exprType)) {
        auto& diag = context.addDiag(diag::ExpressionPatternTypeMismatch, syntax->sourceRange()) << type_;
        diag.addNote(diag::PatternContextType, syntax->sourceRange().start()) << type_;
        diag.addNote(diag::ExpressionPatternType, syntax->sourceRange().start()) << exprType;
        return;
    }
}

TaggedPattern::TaggedPattern(string_view tag, MatchedPattern& subPattern,
                  const TaggedPatternSyntax* syntax) :
        MatchedPattern(PatternKind::Tagged, syntax),
        tag(tag), subPattern(subPattern) {}
TaggedPattern& TaggedPattern::preBind(Compilation& comp, const TaggedPatternSyntax& syntax, const Scope& parentScope) {
    // FIXME: can sub-pattern be empty?
    auto& subPattern = MatchedPattern::preBind(comp, *syntax.pattern, parentScope);
    return *comp.emplace<TaggedPattern>(syntax.memberName.valueText(), subPattern, &syntax);
}

void TaggedPattern::resolve(const BindContext& context, const Type& type_) {
    this->type = &type_;
    const auto& type = type_.getCanonicalType();
    // FIXME: check type is tagged union type
    const Scope* typeScope = nullptr;
    if (PackedUnionType::isKind(type.kind)) {
        typeScope = &type.as<PackedUnionType>();
    }
    else if (UnpackedUnionType::isKind(type.kind)) {
        typeScope = &type.as<UnpackedUnionType>();
    }
    else {
        context.addDiag(diag::TaggedPatternNonTagged, syntax->sourceRange())
            .addNote(diag::PatternContextType, syntax->sourceRange().start()) << type_;
        return;
    }
    const auto& field = typeScope->find<FieldSymbol>(tag);
    // FIXME: checkout for wrong tag
    const auto& memberType = field.getType();
    subPattern.resolve(context, memberType);
}

OrderedStructurePattern::OrderedStructurePattern(span<MatchedPattern*> patterns,
                            const OrderedStructurePatternSyntax* syntax) :
        MatchedPattern(PatternKind::OrderedStructure, syntax),
        patterns(patterns) {}

OrderedStructurePattern& OrderedStructurePattern::preBind(Compilation& comp, const OrderedStructurePatternSyntax& syntax, const Scope& parentScope) {
    SmallVectorSized<MatchedPattern*, 4> patterns;
    for (auto* member: syntax.members) {
        patterns.emplace(&MatchedPattern::preBind(comp, *member->pattern, parentScope));
    }
    return *comp.emplace<OrderedStructurePattern>(patterns.copy(comp), &syntax);
}
void OrderedStructurePattern::resolve(const BindContext& context,
                                                             const Type& type_) {
    this->type = &type_;
    const Type& type = type_.getCanonicalType();
    const Scope* typeScope = nullptr;
    if (PackedStructType::isKind(type.kind)) {
        typeScope = &type.as<PackedStructType>();
    }
    else if (UnpackedStructType::isKind(type.kind)) {
        typeScope = &type.as<UnpackedStructType>();
    }
    else {
        context.addDiag(diag::OrderedStructurePatternNonStruct, syntax->sourceRange())
            .addNote(diag::PatternContextType, syntax->sourceRange().start()) << type_;
        return;
    }
    auto itorRange = typeScope->members();
    auto itor = itorRange.begin(), itorEnd = itorRange.end();
    for (std::size_t i = 0, end = patterns.size(); i < end; ++i) {
        if (itor == itorEnd) {
            context.addDiag(diag::OrderedStructurePatternWrongNumber, patterns[i]->syntax->sourceRange())
                .addNote(diag::PatternContextType, syntax->sourceRange().start()) << type_;
            break;
        }
        patterns[i]->resolve(context, itor->as<FieldSymbol>().getType());
        ++itor;
    }
}

NamedStructurePattern::NamedStructurePattern(span<MemberTy> namedPatterns,
                          const NamedStructurePatternSyntax* syntax) :
        MatchedPattern(PatternKind::NamedStructure, syntax),
        namedPatterns(namedPatterns) {}

NamedStructurePattern& NamedStructurePattern::preBind(Compilation& comp, const NamedStructurePatternSyntax& syntax, const Scope& parentScope) {
    SmallVectorSized<MemberTy, 4> namedPatterns;
    for (auto* member: syntax.members) {
        auto name = member->name.valueText();
        auto* pattern = &MatchedPattern::preBind(comp, *member->pattern, parentScope);
        namedPatterns.emplace(name, pattern);
    }
    return *comp.emplace<NamedStructurePattern>(namedPatterns.copy(comp), &syntax);
}

void NamedStructurePattern::resolve(const BindContext& context, const Type& type_) {
    this->type = &type_;
    const Type& type = type_.getCanonicalType();
    const Scope* typeScope = nullptr;
    if (PackedStructType::isKind(type.kind)) {
        typeScope = &type.as<PackedStructType>();
    }
    else if (UnpackedStructType::isKind(type.kind)) {
        typeScope = &type.as<UnpackedStructType>();
    }
    else {
        context.addDiag(diag::NamedStructurePatternNonStruct, syntax->sourceRange())
            .addNote(diag::PatternContextType, syntax->sourceRange().start()) << type_;
        return;
    }
    for (std::size_t i = 0, end = namedPatterns.size(); i < end; ++i) {
        const auto* field = typeScope->find(namedPatterns[i].first);
        if (!field) {
            auto& diag = context.addDiag(diag::NamedStructurePatternWrongName, namedPatterns[i].second->syntax->sourceRange());
            diag << namedPatterns[i].first;
            diag.addNote(diag::PatternContextType, syntax->sourceRange().start()) << type_;
            break;
        }
        namedPatterns[i].second->resolve(context, field->as<FieldSymbol>().getType());
    }
}

bool ConditionalPredicate::bad() const {
    for (const auto& member : *this) {
        if (member->bad()) {
            return true;
        }
    }
    return false;
}

ConditionalPatternSymbol& ConditionalPatternSymbol::preBind(Compilation& comp, const ConditionalPatternSyntax& syntax, const Scope& parentScope) {
    auto& result = * comp.emplace<ConditionalPatternSymbol>(comp, &syntax);
    parentScope.addMember(result);
    if (syntax.matchesClause) {
        result.pattern = &MatchedPattern::preBind(comp, *syntax.matchesClause->pattern, result);
    }
    return result;
}

bool ConditionalPatternSymbol::bad() const {
    return expr->bad() || (pattern && pattern->bad());
}

void ConditionalPredicate::resolveType(Compilation& comp) {
    for (auto member: *this) {
        if (member->expr->type->isFourState()) {
            type = &comp.getLogicType();
            return;
        }
    }
    type = &comp.getBitType();
}

void PatternCaseItemSymbol::serializeTo(ASTSerializer& serializer) const {
    serializer.write("pattern"sv, *pattern);
    if (expr) {
        serializer.write("expr"sv, *expr);
    }
    else {
        serializer.write("expr"sv, "<no-expression>"sv);
    }
}

ConstantValue ConditionalPredicate::eval(EvalContext& context) const {
    if (size() == 1 && !(*this)[0]->pattern) {
        return (*this)[0]->expr->eval(context);
    }
    // FIXME: better evaluation
    return nullptr;
}

bool ConditionalPredicate::verifyConstant(EvalContext& context) const {
    for (const auto& pred : *this) {
        if (!pred->expr->verifyConstant(context)) {
            return false;
        }
    }
    return true;
}

ConditionalPatternSymbol::ConditionalPatternSymbol(Compilation& comp, const ConditionalPatternSyntax*syntax):
    Symbol(SymbolKind::ConditionalPattern, ""sv, syntax ? syntax->sourceRange().start() : SourceLocation::NoLocation),
    Scope(comp, this),
    syntax(syntax) {
}

void ConditionalPatternSymbol::serializeTo(ASTSerializer& serializer) const {
    serializer.write("expr"sv, *expr);
    if (pattern) {
        serializer.write("pattern"sv, *pattern);
    }
    else {
        serializer.write("pattern"sv, "<no-pattern>"sv);
    }
}

void ConditionalPatternSymbol::resolve(const BindContext& context) {
    expr = &Expression::bind(*syntax->expr, context);
    if (pattern) {
        pattern->resolve(context, *expr->type);
    }
}

ConditionalPredicateInfo& ConditionalPredicateInfo::preBind(Compilation& comp, const ConditionalPredicateSyntax& syntax, const Scope& parentScope) {
    SmallVectorSized<ConditionalPatternSymbol*, 4> patterns;
    const auto* scope = &parentScope;
    for (auto* pattern: syntax.conditions) {
        auto* patSym = &ConditionalPatternSymbol::preBind(comp, *pattern, *scope);
        patterns.emplace(patSym);
        scope = patSym;
    }
    return *comp.emplace<ConditionalPredicateInfo>(&parentScope, patterns.copy(comp));
}

namespace {
    BindContext resetBindContext(const BindContext& context, const Scope& newScope) {
        BindContext newCtx(newScope, LookupLocation::max, context.flags);
        newCtx.evalContext = context.evalContext;
        newCtx.instance = context.instance;
        newCtx.firstIterator = context.firstIterator;
        newCtx.classRandomizeScope = context.classRandomizeScope;
        return newCtx;
    }
}

BindContext ConditionalPredicateInfo::getLastBindContext(const BindContext& context) {
    return resetBindContext(context, getLastScope());
}

ConditionalPredicate& ConditionalPredicateInfo::resolveConditionalPredicate(const BindContext& context) {
    Compilation& comp = context.scope.getCompilation();
    std::unique_ptr<BindContext> ctx{new BindContext(context)};
    auto& result = *comp.emplace<ConditionalPredicate>(patterns);
    for (auto member: result) {
        member->resolve(*ctx);
        auto newCtx = new BindContext(resetBindContext(*ctx, *member));
        ctx.reset(newCtx);
    }
    result.resolveType(context.scope.getCompilation());
    return result;
}

PatternCaseItemSymbol::PatternCaseItemSymbol(Compilation& comp,
                          const PatternCaseItemSyntax* syntax) :
    Symbol(SymbolKind::PatternCaseItem, ""sv, syntax ? syntax->sourceRange().start() : SourceLocation::NoLocation),
    Scope(comp, this), syntax(syntax) {
}

PatternCaseItemSymbol& PatternCaseItemSymbol::preBind(Compilation& comp, const PatternCaseItemSyntax& syntax, const Scope& parentScope) {
    auto& result = *comp.emplace<PatternCaseItemSymbol>(comp, &syntax);
    parentScope.addMember(result);
    result.pattern = &MatchedPattern::preBind(comp, *syntax.pattern, result);
    return result;
}

void PatternCaseItemSymbol::resolve(const BindContext& context, const Expression& matchedExpr) {
    this->matchedExpr = &matchedExpr;
    pattern->resolve(context, *matchedExpr.type);
    if (syntax->expr) {
        expr = &Expression::bind(*syntax->expr, getBindContext(context));
    }
}

BindContext PatternCaseItemSymbol::getBindContext(const BindContext& context) {
    return resetBindContext(context, *this);
}

PatternCaseItemInfo& PatternCaseItemInfo::preBind(Compilation& comp, const PatternCaseItemSyntax& syntax, const Scope& parentScope) {
    auto& caseItem = PatternCaseItemSymbol::preBind(comp, syntax, parentScope);
    auto& result = *comp.emplace<PatternCaseItemInfo>(&parentScope, &caseItem);
    return result;
}

PatternCaseItemSymbol& PatternCaseItemInfo::resolvePatternCaseItem(const BindContext& context, const Expression& matchedExpr) {
    caseItem->resolve(context, matchedExpr);
    return *caseItem;
}

} // namespace slang

