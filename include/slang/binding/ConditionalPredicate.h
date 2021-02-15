#pragma once

#include <type_traits>

#include "slang/binding/Statements.h"
#include "slang/symbols/ASTSerializer.h"
#include "slang/symbols/Scope.h"
#include "slang/symbols/Symbol.h"

namespace slang {

class BindContext;
class Compilation;
class EvalContext;
class Expression;
class PatternBindingSymbol;
class Type;

struct PatternSyntax;
struct WildcardPatternSyntax;
struct VariablePatternSyntax;
struct ExpressionPatternSyntax;
struct ParenthesizedPatternSyntax;
struct TaggedPatternSyntax;
struct OrderedStructurePatternSyntax;
struct NamedStructurePatternSyntax;

// No ParenthesizedPattern in AST, the parentheses are stripped away
// clang-format off
#define PATTERN(x) \
    x(Wildcard) \
    x(Variable) \
    x(Expression) \
    x(Tagged) \
    x(OrderedStructure) \
    x(NamedStructure)
ENUM(PatternKind, PATTERN)
#undef PATTERN
// clang-format on

/// Base class of all kinds of patterns
class MatchedPattern {
public:
    PatternKind kind;
    const Type* type = nullptr;
    const PatternSyntax* syntax;
    SourceLocation location;
    MatchedPattern(PatternKind kind, const PatternSyntax* syntax);
    // In preBind, the pattern's AST nodes are created and pattern-binding symbols are added to proper scope. But the pattern's type is not determined and semantical rules are not fully checked, thoese things are deferred to resolve
    static MatchedPattern& preBind(Compilation& comp, const PatternSyntax& syntax, const Scope& parentScope);
    // Propogate and determine a pattern's type, and do various semantical check.
    void resolve(const BindContext& context, const Type& type);
    bool bad() const;
    static const PatternSyntax& stripAwayParentheses(const ParenthesizedPatternSyntax& syntax);
    template<typename TVisitor, typename... Args>
    decltype(auto) visit(TVisitor&& visitor, Args&&... args) const;
    template<typename Derived>
    std::enable_if_t<std::is_base_of_v<MatchedPattern, Derived>, Derived&> as() {
        return static_cast<Derived&>(*this);
    }
    template<typename Derived>
    std::enable_if_t<std::is_base_of_v<MatchedPattern, Derived>, const Derived&> as() const {
        return static_cast<const Derived&>(*this);
    }
};

class WildcardPattern : public MatchedPattern {
public:
    WildcardPattern(const WildcardPatternSyntax* syntax);
    static WildcardPattern& preBind(Compilation& comp, const WildcardPatternSyntax& syntax);
    void resolve(const BindContext& context, const Type& type);
};

/// variable-pattern creates varaible binding, the scope of PatternBinding symbols is contained in
/// the ConditionalPatternSymbol or PatternCaseItemSymbol
class VariablePattern : public MatchedPattern {
public:
    PatternBindingSymbol& symbol;
    VariablePattern(const VariablePatternSyntax* syntax, PatternBindingSymbol& symbol);
    static VariablePattern& preBind(Compilation& comp, const VariablePatternSyntax& syntax, const Scope& parentScope);
    void resolve(const BindContext& context, const Type& type);
};

class ExpressionPattern : public MatchedPattern {
public:
    ExpressionPattern(const ExpressionPatternSyntax* syntax);
    const Expression* expr = nullptr;
    static ExpressionPattern& preBind(Compilation& comp, const ExpressionPatternSyntax& syntax);
    void resolve(const BindContext& context, const Type& type);
};

class TaggedPattern : public MatchedPattern {
public:
    string_view tag;
    MatchedPattern& subPattern;
    TaggedPattern(string_view tag, MatchedPattern& subPattern,
                  const TaggedPatternSyntax* syntax);
    static TaggedPattern& preBind(Compilation& comp, const TaggedPatternSyntax& syntax, const Scope& parentScope);
    void resolve(const BindContext& context, const Type& type);
};

class OrderedStructurePattern : public MatchedPattern {
public:
    span<MatchedPattern*> patterns;
    OrderedStructurePattern(span<MatchedPattern*> patterns,
                            const OrderedStructurePatternSyntax* syntax);
    static OrderedStructurePattern& preBind(Compilation& comp, const OrderedStructurePatternSyntax& syntax, const Scope& parentScope);
    void resolve(const BindContext& context, const Type& type);
};

class NamedStructurePattern : public MatchedPattern {
public:
    using MemberTy = std::pair<string_view, MatchedPattern*>;
    span<MemberTy> namedPatterns;
    NamedStructurePattern(span<MemberTy> namedPatterns,
                          const NamedStructurePatternSyntax* syntax);
    static NamedStructurePattern& preBind(Compilation& comp, const NamedStructurePatternSyntax& syntax, const Scope& parentScope);
    void resolve(const BindContext& context, const Type& type);
};

///  ConditionalPatternSymbol ::= expr [matches pattern]
struct ConditionalPatternSyntax;
/// ConditionalPredicate ::= list of ConditionalPatternSymbol
struct ConditionalPredicateSyntax;

/// ConditionalPatternSymbols is a scope containing all the pattern-bindings in its pattern
class ConditionalPatternSymbol: public Symbol, public Scope {
public:
    const Expression* expr = nullptr;
    MatchedPattern* pattern = nullptr;
    const ConditionalPatternSyntax* syntax;
    ConditionalPatternSymbol(Compilation& comp, const ConditionalPatternSyntax* syntax);
    static ConditionalPatternSymbol& preBind(Compilation& comp, const ConditionalPatternSyntax& syntax, const Scope& parentScope);
    void resolve(const BindContext& context);
    void serializeTo(ASTSerializer& serializer) const;
    bool bad() const;
    template<typename TVisitor, typename... Args>
    void visit(TVisitor&& visitor, Args&&... args) const;
};

// a ConditionalPredicate is basically a list of ConditionalPatternSymbols, the ConditionalPatternSymbols' scopes are nested within each other
class ConditionalPredicate: public ::nonstd::span<ConditionalPatternSymbol*>{
public:
    using BaseSpan = ::nonstd::span<ConditionalPatternSymbol*>;
    ConditionalPredicate(BaseSpan span) :
        BaseSpan(span) {}

    /// one bit Logic/Bit type of the final result
    const Type* type = nullptr;

    // determines this ConditionalPredicate's type (bit or logic)
    void resolveType(Compilation& comp);
    bool bad() const;
    template<typename TVisitor, typename... Args>
    void visit(TVisitor&& visitor, Args&&... args) const {
        std::size_t size = this->size();
        for (std::size_t i = 0; i < size; ++i) {
            (*this)[i]->visit(std::forward<TVisitor>(visitor), std::forward<Args>(args)...);
        }
    }

    /// Evaluates the conditional predicate under the given evaluation context. Any errors that
    /// occur will be stored in the evaluation context instead of issued to the compilation.
    ConstantValue eval(EvalContext& context) const;

    /// Verifies that this conditional predicate evaluates to a known value at compile time
    // FIXME: issue diagnostics to where?
    bool verifyConstant(EvalContext& context) const;

    void serializeTo(ASTSerializer& serializer) const;
};

// a helper class used to build a ConditionalPredicate
struct ConditionalPredicateInfo {
    const Scope* parentScope;
    span<ConditionalPatternSymbol*> patterns;
    StatementBinder binder;
    ConditionalPredicateInfo(const Scope* parentScope, span<ConditionalPatternSymbol*> patterns): parentScope(parentScope), patterns(patterns) {}
    const Scope& getLastScope() {
        if (!patterns.empty()) {
            return *patterns[patterns.size() - 1];
        } else {
            return *parentScope;
        }
    }
    BindContext getLastBindContext(const BindContext& context);
    ConditionalPredicate& resolveConditionalPredicate(const BindContext& context);
    static ConditionalPredicateInfo& preBind(Compilation& comp, const ConditionalPredicateSyntax& syntax, const Scope& parentScope);
};

/// PatternCaseItemSymbol ::= pattern [ &&& expr ]
struct PatternCaseItemSyntax;

/// PatternCaseItemSymbol is a scope containing all the pattern-bindings in its pattern
class PatternCaseItemSymbol: public Symbol, public Scope {
public:
    MatchedPattern* pattern = nullptr;
    const Expression* expr = nullptr;
    const Expression* matchedExpr = nullptr;
    const PatternCaseItemSyntax* syntax;
    PatternCaseItemSymbol(Compilation& comp,
                          const PatternCaseItemSyntax* syntax);
    static PatternCaseItemSymbol& preBind(Compilation& comp, const PatternCaseItemSyntax& syntax, const Scope& parentScope);
    void resolve(const BindContext& context, const Expression& matchedExpr);
    template<typename TVisitor, typename... Args>
    void visit(TVisitor&& visitor, Args&&... args) const;
    /// Evaluates the pattern case item under the given evaluation context. Any errors that
    /// occur will be stored in the evaluation context instead of issued to the compilation.
    ConstantValue eval(EvalContext& context) const;

    /// Verifies that this pattern case item evaluates to a known value at compile time
    // FIXME: issue diganostics to what place?
    bool verifyConstant(EvalContext& context) const;
    void serializeTo(ASTSerializer& serializer) const;
    BindContext getBindContext(const BindContext& context);
};

// a helper class used to build a PatternCaseItemSymbol
struct PatternCaseItemInfo {
    PatternCaseItemInfo(const Scope* parentScope, PatternCaseItemSymbol* caseItem): parentScope(parentScope), caseItem(caseItem) {}
    const Scope* parentScope;
    PatternCaseItemSymbol* caseItem;
    StatementBinder binder;
    const Scope& getScope() {
        return *caseItem;
    }
    PatternCaseItemSymbol& resolvePatternCaseItem(const BindContext& context, const Expression& matchedExpr);
    static PatternCaseItemInfo& preBind(Compilation& comp, const PatternCaseItemSyntax& syntax, const Scope& parentScope);
};

} // namespace slang

