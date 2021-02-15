//------------------------------------------------------------------------------
// Parser_expressions.cpp
// Expression-related parsing methods
//
// File is under the MIT license; see LICENSE for details
//------------------------------------------------------------------------------
#include "slang/diagnostics/ParserDiags.h"
#include "slang/parsing/Lexer.h"
#include "slang/parsing/Parser.h"
#include "slang/parsing/Preprocessor.h"
#include "slang/util/ScopeGuard.h"

namespace slang {

ExpressionSyntax& Parser::parseExpression() {
    return parseSubExpression(ExpressionOptions::None, 0);
}

ExpressionSyntax& Parser::parseMinTypMaxExpression() {
    ExpressionSyntax& first = parseExpression();
    if (!peek(TokenKind::Colon))
        return first;

    auto colon1 = consume();
    auto& typ = parseExpression();
    auto colon2 = expect(TokenKind::Colon);
    auto& max = parseExpression();

    return factory.minTypMaxExpression(first, colon1, typ, colon2, max);
}

static bool isNewExpr(const ExpressionSyntax* expr) {
    while (true) {
        if (expr->kind == SyntaxKind::ConstructorName)
            return true;

        if (expr->kind != SyntaxKind::ScopedName)
            return false;

        expr = expr->as<ScopedNameSyntax>().right;
    }
}

ExpressionSyntax& Parser::parseSubExpression(bitmask<ExpressionOptions> options, int precedence) {
    auto dg = setDepthGuard();

    auto current = peek();
    if (isPossibleDelayOrEventControl(current.kind)) {
        auto timingControl = parseTimingControl(/* isSequenceExpr */ false);
        ASSERT(timingControl);

        auto& expr = factory.timingControlExpression(*timingControl, parseExpression());
        return parsePostfixExpression(expr);
    }
    else if (current.kind == TokenKind::TaggedKeyword) {
        // TODO: check for trailing expression
        auto tagged = consume();
        auto member = expect(TokenKind::Identifier);
        return factory.taggedUnionExpression(tagged, member, nullptr);
    }

    ExpressionSyntax* leftOperand;
    SyntaxKind opKind = getUnaryPrefixExpression(current.kind);
    if (opKind != SyntaxKind::Unknown)
        leftOperand = &parsePrefixExpression(options, opKind);
    else {
        leftOperand = &parsePrimaryExpression(false);

        // If the primary is a new or scoped new operator we should handle
        // that separately (it doesn't participate in postfix expression parsing).
        if (isNewExpr(leftOperand))
            return parseNewExpression(leftOperand->as<NameSyntax>(), options);

        leftOperand = &parsePostfixExpression(*leftOperand);
    }

    options &= ~ExpressionOptions::AllowSuperNewCall;

    while (true) {
        // either a binary operator, or we're done
        current = peek();
        opKind = getBinaryExpression(current.kind);
        if (opKind == SyntaxKind::Unknown)
            break;

        // the "or" operator and "iff" clause in event expressions are special,
        // we don't handle them here
        if ((opKind == SyntaxKind::OrSequenceExpression ||
             opKind == SyntaxKind::IffPropertyExpression) &&
            options.has(ExpressionOptions::EventExpressionContext)) {
            break;
        }

        // the implication operator in constraint blocks is special, we don't handle it here
        if (opKind == SyntaxKind::LogicalImplicationExpression &&
            (options & ExpressionOptions::ConstraintContext)) {
            break;
        }

        // we have to special case '<=', which can be less than or nonblocking assignment depending
        // on context
        if (opKind == SyntaxKind::LessThanEqualExpression &&
            (options & ExpressionOptions::ProceduralAssignmentContext)) {
            opKind = SyntaxKind::NonblockingAssignmentExpression;
        }
        options &= ~ExpressionOptions::ProceduralAssignmentContext;

        // see if we should take this operator or if it's part of our parent due to precedence
        int newPrecedence = getPrecedence(opKind);
        if (newPrecedence < precedence)
            break;

        // if we have a precedence tie, check associativity
        if (newPrecedence == precedence && !isRightAssociative(opKind))
            break;

        // take the operator
        if (opKind == SyntaxKind::InsideExpression)
            leftOperand = &parseInsideExpression(*leftOperand);
        else {
            auto opToken = consume();
            auto attributes = parseAttributes();
            auto& rightOperand = parseSubExpression(options, newPrecedence);
            leftOperand =
                &factory.binaryExpression(opKind, *leftOperand, opToken, attributes, rightOperand);
        }
    }

    // Handle conditional expressions (and their optional pattern matched predicate).
    // Only do this if we're not already within a conditional pattern context, and if
    // we're at the right precedence level (one lower than a logical-or) to take it.
    int logicalOrPrecedence = getPrecedence(SyntaxKind::LogicalOrExpression);
    if ((options & ExpressionOptions::PatternContext) == 0 && precedence < logicalOrPrecedence) {
        // If this is the start of a pattern predicate, check whether there's actually a
        // question mark coming up. Otherwise we might be a predicate inside a
        // statement which doesn't need the question.
        bool takeConditional = current.kind == TokenKind::Question;
        if (current.kind == TokenKind::MatchesKeyword || current.kind == TokenKind::TripleAnd)
            takeConditional = isConditionalExpression();

        if (takeConditional) {
            Token question;
            auto& predicate =
                parseConditionalPredicate(*leftOperand, TokenKind::Question, question);
            auto attributes = parseAttributes();
            auto& left = parseSubExpression(options, logicalOrPrecedence - 1);
            auto colon = expect(TokenKind::Colon);
            auto& right = parseSubExpression(options, logicalOrPrecedence - 1);
            leftOperand =
                &factory.conditionalExpression(predicate, question, attributes, left, colon, right);
        }
    }

    return *leftOperand;
}

ExpressionSyntax& Parser::parsePrefixExpression(bitmask<ExpressionOptions> options,
                                                SyntaxKind opKind) {
    switch (opKind) {
        case SyntaxKind::UnarySequenceDelayExpression:
        case SyntaxKind::UnarySequenceEventExpression: {
            auto timing = parseTimingControl(/* isSequenceExpr */ true);
            ASSERT(timing);
            return factory.timingControlExpression(*timing, parseExpression());
        }
        case SyntaxKind::NextTimePropertyExpression:
        case SyntaxKind::SNextTimePropertyExpression:
        case SyntaxKind::AlwaysPropertyExpression:
        case SyntaxKind::SAlwaysPropertyExpression:
        case SyntaxKind::EventuallyPropertyExpression:
        case SyntaxKind::SEventuallyPropertyExpression:
            // TODO:
            break;
        case SyntaxKind::AcceptOnPropertyExpression:
        case SyntaxKind::RejectOnPropertyExpression:
        case SyntaxKind::SyncAcceptOnPropertyExpression:
        case SyntaxKind::SyncRejectOnPropertyExpression:
            // TODO:
            break;
        default:
            break;
    }

    auto opToken = consume();
    auto attributes = parseAttributes();

    ExpressionSyntax& operand = parseSubExpression(options, getPrecedence(opKind));
    return factory.prefixUnaryExpression(opKind, opToken, attributes, operand);
}

ExpressionSyntax& Parser::parsePrimaryExpression(bool disallowVector) {
    TokenKind kind = peek().kind;
    switch (kind) {
        case TokenKind::StringLiteral:
        case TokenKind::UnbasedUnsizedLiteral:
        case TokenKind::NullKeyword:
        case TokenKind::OneStep:
        case TokenKind::Dollar: {
            auto literal = consume();
            return factory.literalExpression(getLiteralExpression(literal.kind), literal);
        }
        case TokenKind::TimeLiteral:
            return factory.literalExpression(SyntaxKind::TimeLiteralExpression,
                                             numberParser.parseReal(*this));
        case TokenKind::RealLiteral:
            return factory.literalExpression(SyntaxKind::RealLiteralExpression,
                                             numberParser.parseReal(*this));
        case TokenKind::IntegerLiteral:
        case TokenKind::IntegerBase:
            return parseIntegerExpression(disallowVector);
        case TokenKind::OpenParenthesis: {
            auto openParen = consume();
            auto expr = &parseMinTypMaxExpression();

            auto closeParen = expect(TokenKind::CloseParenthesis);
            return factory.parenthesizedExpression(openParen, *expr, closeParen);
        }
        case TokenKind::ApostropheOpenBrace:
            return parseAssignmentPatternExpression(nullptr);
        case TokenKind::OpenBrace: {
            // several different things this could be:
            // 1. empty queue expression { }
            // 2. streaming concatenation {>> {expr}}
            // 3. multiple concatenation {expr {concat}}
            // 4. concatenation {expr, expr}
            auto openBrace = consume();
            switch (peek().kind) {
                case TokenKind::CloseBrace:
                    return factory.emptyQueueExpression(openBrace, consume());
                case TokenKind::LeftShift:
                case TokenKind::RightShift:
                    return parseStreamConcatenation(openBrace);
                default: {
                    auto& first = parseExpression();
                    if (!peek(TokenKind::OpenBrace))
                        return parseConcatenation(openBrace, &first);
                    else {
                        auto openBraceInner = consume();
                        auto& concat = parseConcatenation(openBraceInner, nullptr);
                        auto closeBrace = expect(TokenKind::CloseBrace);
                        return factory.multipleConcatenationExpression(openBrace, first, concat,
                                                                       closeBrace);
                    }
                }
            }
        }
        case TokenKind::SignedKeyword:
        case TokenKind::UnsignedKeyword:
        case TokenKind::ConstKeyword: {
            auto signing = consume();
            auto apostrophe = expect(TokenKind::Apostrophe);
            auto openParen = expect(TokenKind::OpenParenthesis);
            auto& innerExpr = parseExpression();
            auto closeParen = expect(TokenKind::CloseParenthesis);
            auto& parenExpr = factory.parenthesizedExpression(openParen, innerExpr, closeParen);
            return factory.signedCastExpression(signing, apostrophe, parenExpr);
        }
        case TokenKind::SystemIdentifier:
            return factory.systemName(consume());
        default:
            // possibilities here:
            // 1. data type
            // 2. qualified name
            // 3. implicit class handles
            // 4. any of [1-3] with an assignment pattern
            // 5. any of [1-3] with a cast expression
            // 6. error
            if (isPossibleDataType(kind) && kind != TokenKind::Identifier &&
                kind != TokenKind::UnitSystemName) {

                auto& type = parseDataType();
                if (peek(TokenKind::ApostropheOpenBrace))
                    return parseAssignmentPatternExpression(&type);
                else
                    return type;
            }
            else {
                // parseName() will insert a missing identifier token for the error case
                auto& name = parseName(NameOptions::ExpectingExpression);
                if (peek(TokenKind::ApostropheOpenBrace))
                    return parseAssignmentPatternExpression(&factory.namedType(name));
                else {
                    // otherwise just a name expression
                    return name;
                }
            }
    }
}

ExpressionSyntax& Parser::parseIntegerExpression(bool disallowVector) {
    auto result =
        disallowVector ? numberParser.parseSimpleInt(*this) : numberParser.parseInteger(*this);

    if (result.isSimple)
        return factory.literalExpression(SyntaxKind::IntegerLiteralExpression, result.value);

    return factory.integerVectorExpression(result.size, result.base, result.value);
}

void Parser::handleExponentSplit(Token token, size_t offset) {
    SmallVectorSized<Token, 4> split;
    Lexer::splitTokens(alloc, getDiagnostics(), getPP().getSourceManager(), token, offset,
                       getPP().getCurrentKeywordVersion(), split);

    pushTokens(split);
}

ExpressionSyntax& Parser::parseInsideExpression(ExpressionSyntax& expr) {
    auto inside = expect(TokenKind::InsideKeyword);
    auto& list = parseOpenRangeList();
    return factory.insideExpression(expr, inside, list);
}

OpenRangeListSyntax& Parser::parseOpenRangeList() {
    Token openBrace;
    Token closeBrace;
    span<TokenOrSyntax> list;

    parseList<isPossibleOpenRangeElement, isEndOfBracedList>(
        TokenKind::OpenBrace, TokenKind::CloseBrace, TokenKind::Comma, openBrace, list, closeBrace,
        RequireItems::True, diag::ExpectedOpenRangeElement,
        [this] { return &parseOpenRangeElement(); });

    return factory.openRangeList(openBrace, list, closeBrace);
}

ExpressionSyntax& Parser::parseOpenRangeElement() {
    if (!peek(TokenKind::OpenBracket))
        return parseExpression();

    auto openBracket = consume();
    auto& left = parseExpression();
    auto colon = expect(TokenKind::Colon);
    auto& right = parseExpression();
    auto closeBracket = expect(TokenKind::CloseBracket);
    return factory.openRangeExpression(openBracket, left, colon, right, closeBracket);
}

ConcatenationExpressionSyntax& Parser::parseConcatenation(Token openBrace,
                                                          ExpressionSyntax* first) {
    SmallVectorSized<TokenOrSyntax, 8> buffer;
    if (first) {
        // it's possible to have just one element in the concatenation list, so check for a close
        // brace
        buffer.append(first);
        if (peek(TokenKind::CloseBrace))
            return factory.concatenationExpression(openBrace, buffer.copy(alloc), consume());

        buffer.append(expect(TokenKind::Comma));
    }

    Token closeBrace;
    parseList<isPossibleExpressionOrComma, isEndOfBracedList>(
        buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace, RequireItems::False,
        diag::ExpectedExpression, [this] { return &parseExpression(); });
    return factory.concatenationExpression(openBrace, buffer.copy(alloc), closeBrace);
}

StreamingConcatenationExpressionSyntax& Parser::parseStreamConcatenation(Token openBrace) {
    auto op = consume();
    ExpressionSyntax* sliceSize = nullptr;
    if (!peek(TokenKind::OpenBrace))
        sliceSize = &parseExpression();

    Token openBraceInner;
    Token closeBraceInner;
    span<TokenOrSyntax> list;

    parseList<isPossibleExpressionOrComma, isEndOfBracedList>(
        TokenKind::OpenBrace, TokenKind::CloseBrace, TokenKind::Comma, openBraceInner, list,
        closeBraceInner, RequireItems::True, diag::ExpectedStreamExpression,
        [this] { return &parseStreamExpression(); });

    auto closeBrace = expect(TokenKind::CloseBrace);
    return factory.streamingConcatenationExpression(openBrace, op, sliceSize, openBraceInner, list,
                                                    closeBraceInner, closeBrace);
}

StreamExpressionSyntax& Parser::parseStreamExpression() {
    auto& expr = parseExpression();

    StreamExpressionWithRangeSyntax* withRange = nullptr;
    if (peek(TokenKind::WithKeyword)) {
        auto with = consume();
        withRange = &factory.streamExpressionWithRange(with, parseElementSelect());
    }

    return factory.streamExpression(expr, withRange);
}

AssignmentPatternExpressionSyntax& Parser::parseAssignmentPatternExpression(DataTypeSyntax* type) {
    auto openBrace = expect(TokenKind::ApostropheOpenBrace);

    // we either have an expression here, or the default keyword for a pattern key
    ExpressionSyntax* firstExpr;
    if (peek(TokenKind::DefaultKeyword)) {
        firstExpr = &factory.literalExpression(SyntaxKind::DefaultPatternKeyExpression, consume());
    }
    else if (peek(TokenKind::CloseBrace)) {
        // This is an empty pattern -- we'll just warn and continue on.
        addDiag(diag::EmptyAssignmentPattern, openBrace.location());

        auto pattern =
            &factory.simpleAssignmentPattern(openBrace, span<TokenOrSyntax>{}, consume());
        return factory.assignmentPatternExpression(type, *pattern);
    }
    else {
        firstExpr = &parseExpression();
    }

    Token closeBrace;
    AssignmentPatternSyntax* pattern;
    SmallVectorSized<TokenOrSyntax, 8> buffer;

    switch (peek().kind) {
        case TokenKind::Colon:
            buffer.append(&parseAssignmentPatternItem(firstExpr));
            if (peek(TokenKind::Comma)) {
                buffer.append(consume());

                parseList<isPossibleExpressionOrCommaOrDefault, isEndOfBracedList>(
                    buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace,
                    RequireItems::False, diag::ExpectedAssignmentKey,
                    [this] { return &parseAssignmentPatternItem(nullptr); });
            }
            else {
                closeBrace = expect(TokenKind::CloseBrace);
            }

            pattern =
                &factory.structuredAssignmentPattern(openBrace, buffer.copy(alloc), closeBrace);
            break;
        case TokenKind::OpenBrace: {
            auto innerOpenBrace = consume();

            parseList<isPossibleExpressionOrComma, isEndOfBracedList>(
                buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace, RequireItems::True,
                diag::ExpectedExpression, [this] { return &parseExpression(); });
            pattern = &factory.replicatedAssignmentPattern(openBrace, *firstExpr, innerOpenBrace,
                                                           buffer.copy(alloc), closeBrace,
                                                           expect(TokenKind::CloseBrace));
            break;
        }
        case TokenKind::Comma:
            buffer.append(firstExpr);
            buffer.append(consume());

            parseList<isPossibleExpressionOrComma, isEndOfBracedList>(
                buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace, RequireItems::True,
                diag::ExpectedExpression, [this] { return &parseExpression(); });
            pattern = &factory.simpleAssignmentPattern(openBrace, buffer.copy(alloc), closeBrace);
            break;
        case TokenKind::CloseBrace:
            buffer.append(firstExpr);
            closeBrace = consume();
            pattern = &factory.simpleAssignmentPattern(openBrace, buffer.copy(alloc), closeBrace);
            break;
        default:
            // This is an error case; let the list handling code get us out of it.
            buffer.append(firstExpr);
            buffer.append(expect(TokenKind::Comma));

            parseList<isPossibleExpressionOrComma, isEndOfBracedList>(
                buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace, RequireItems::False,
                diag::ExpectedExpression, [this] { return &parseExpression(); });
            pattern = &factory.simpleAssignmentPattern(openBrace, buffer.copy(alloc), closeBrace);
            break;
    }
    ASSERT(pattern);
    return factory.assignmentPatternExpression(type, *pattern);
}

AssignmentPatternItemSyntax& Parser::parseAssignmentPatternItem(ExpressionSyntax* key) {
    if (!key) {
        if (peek(TokenKind::DefaultKeyword))
            key = &factory.literalExpression(SyntaxKind::DefaultPatternKeyExpression, consume());
        else
            key = &parseExpression();
    }

    auto colon = expect(TokenKind::Colon);
    return factory.assignmentPatternItem(*key, colon, parseExpression());
}

ElementSelectSyntax& Parser::parseElementSelect() {
    auto openBracket = expect(TokenKind::OpenBracket);
    auto selector = parseElementSelector();
    auto closeBracket = expect(TokenKind::CloseBracket);
    return factory.elementSelect(openBracket, selector, closeBracket);
}

SelectorSyntax* Parser::parseElementSelector() {
    if (peek().kind == TokenKind::CloseBracket) {
        return nullptr;
    }
    auto& expr = parseExpression();
    switch (peek().kind) {
        case TokenKind::Colon: {
            auto range = consume();
            return &factory.rangeSelect(SyntaxKind::SimpleRangeSelect, expr, range,
                                        parseExpression());
        }
        case TokenKind::PlusColon: {
            auto range = consume();
            return &factory.rangeSelect(SyntaxKind::AscendingRangeSelect, expr, range,
                                        parseExpression());
        }
        case TokenKind::MinusColon: {
            auto range = consume();
            return &factory.rangeSelect(SyntaxKind::DescendingRangeSelect, expr, range,
                                        parseExpression());
        }
        default:
            return &factory.bitSelect(expr);
    }
}

ExpressionSyntax& Parser::parsePostfixExpression(ExpressionSyntax& lhs) {
    ExpressionSyntax* expr = &lhs;
    while (true) {
        switch (peek().kind) {
            case TokenKind::OpenBracket:
                expr = &factory.elementSelectExpression(*expr, parseElementSelect());
                break;
            case TokenKind::Dot: {
                auto dot = consume();
                auto name = expect(TokenKind::Identifier);
                expr = &factory.memberAccessExpression(*expr, dot, name);
                break;
            }
            case TokenKind::OpenParenthesis:
                expr = &factory.invocationExpression(*expr, nullptr, &parseArgumentList());
                break;
            case TokenKind::DoublePlus:
            case TokenKind::DoubleMinus: {
                // can't have any other postfix expressions after inc/dec
                auto op = consume();
                return factory.postfixUnaryExpression(getUnaryPostfixExpression(op.kind), *expr,
                                                      nullptr, op);
            }
            case TokenKind::Apostrophe: {
                auto apostrophe = consume();
                auto openParen = expect(TokenKind::OpenParenthesis);
                auto& innerExpr = parseExpression();
                auto closeParen = expect(TokenKind::CloseParenthesis);
                auto& parenExpr = factory.parenthesizedExpression(openParen, innerExpr, closeParen);
                expr = &factory.castExpression(*expr, apostrophe, parenExpr);
                break;
            }
            case TokenKind::OpenParenthesisStar: {
                auto attributes = parseAttributes();
                switch (peek().kind) {
                    case TokenKind::DoublePlus:
                    case TokenKind::DoubleMinus: {
                        auto op = consume();
                        return factory.postfixUnaryExpression(getUnaryPostfixExpression(op.kind),
                                                              *expr, attributes, op);
                    }
                    case TokenKind::OpenParenthesis:
                        expr =
                            &factory.invocationExpression(*expr, attributes, &parseArgumentList());
                        break;
                    default:
                        // otherwise, this has to be a function call without any arguments
                        expr = &factory.invocationExpression(*expr, attributes, nullptr);
                        break;
                }
                break;
            }
            case TokenKind::WithKeyword:
                // If we see bracket right after the with keyword, this is actually part of a stream
                // expression -- return and let the call further up the stack handle it.
                if (peek(1).kind == TokenKind::OpenBracket)
                    return *expr;
                expr = &parseArrayOrRandomizeMethod(*expr);
                break;
            case TokenKind::DoubleHash: {
                auto timing = parseTimingControl(/* isSequenceExpr */ true);
                ASSERT(timing);
                expr = &factory.timingControlExpressionConcatenation(*expr, *timing,
                                                                     parseExpression());
                break;
            }
            default:
                return *expr;
        }
    }
}

NameSyntax& Parser::parseName() {
    return parseName(NameOptions::None);
}

NameSyntax& Parser::parseName(bitmask<NameOptions> options) {
    NameSyntax* name = &parseNamePart(options | NameOptions::IsFirst);
    options &= ~NameOptions::ExpectingExpression;

    bool usedDot = false;
    bool reportedError = false;
    SyntaxKind previousKind = name->kind;

    auto kind = peek().kind;
    while (kind == TokenKind::Dot || kind == TokenKind::DoubleColon) {
        auto separator = consume();
        if (kind == TokenKind::Dot)
            usedDot = true;
        else if (usedDot && !reportedError) {
            reportedError = true;
            addDiag(diag::InvalidAccessDotColon, separator.location()) << "::"sv
                                                                       << "."sv;
        }

        switch (previousKind) {
            case SyntaxKind::UnitScope:
            case SyntaxKind::LocalScope:
                if (kind != TokenKind::DoubleColon) {
                    addDiag(diag::InvalidAccessDotColon, separator.location()) << "."sv
                                                                               << "::"sv;
                }
                break;
            case SyntaxKind::RootScope:
            case SyntaxKind::ThisHandle:
            case SyntaxKind::SuperHandle:
                if (kind != TokenKind::Dot) {
                    addDiag(diag::InvalidAccessDotColon, separator.location()) << "::"sv
                                                                               << "."sv;
                }
                break;
            case SyntaxKind::ConstructorName:
                addDiag(diag::NewKeywordQualified, separator.location());
                break;
            default:
                break;
        }

        bitmask<NameOptions> nextOptions = options;
        if (previousKind == SyntaxKind::ThisHandle)
            nextOptions |= NameOptions::PreviousWasThis;
        else if (previousKind == SyntaxKind::LocalScope)
            nextOptions |= NameOptions::PreviousWasLocal;

        NameSyntax& rhs = parseNamePart(nextOptions);
        previousKind = rhs.kind;

        name = &factory.scopedName(*name, separator, rhs);
        kind = peek().kind;
    }

    // If we saw $unit, $root, super, or local, make sure the correct token follows it.
    TokenKind expectedKind = TokenKind::Unknown;
    switch (name->kind) {
        case SyntaxKind::UnitScope:
        case SyntaxKind::LocalScope:
            expectedKind = TokenKind::DoubleColon;
            break;
        case SyntaxKind::RootScope:
        case SyntaxKind::SuperHandle:
            expectedKind = TokenKind::Dot;
            break;
        default:
            break;
    }

    if (expectedKind != TokenKind::Unknown) {
        auto separator = expect(expectedKind);
        name = &factory.scopedName(*name, separator, parseNamePart(options));
    }

    return *name;
}

NameSyntax& Parser::parseNamePart(bitmask<NameOptions> options) {
    auto kind = getKeywordNameExpression(peek().kind);
    if (kind != SyntaxKind::Unknown) {
        // This is a keyword name such as "super", "xor", or "new".
        bool isFirst = (options & NameOptions::IsFirst) != 0;
        if (isSpecialMethodName(kind)) {
            // The built-in methods ("xor", "unique", etc) and are not allowed
            // to be the first element in the name.
            if (!isFirst)
                return factory.keywordName(kind, consume());
        }
        else if (kind == SyntaxKind::ConstructorName) {
            // "new" names are always allowed.
            return factory.keywordName(kind, consume());
        }
        else {
            // Otherwise this is "$unit", "$root", "local", "this", "super".
            // These are only allowed to be the first element in a path, except
            // for "super" which can follow "this".
            if (isFirst ||
                (kind == SyntaxKind::SuperHandle && options.has(NameOptions::PreviousWasThis)) ||
                ((kind == SyntaxKind::SuperHandle || kind == SyntaxKind::ThisHandle) &&
                 options.has(NameOptions::PreviousWasLocal))) {
                return factory.keywordName(kind, consume());
            }
        }

        // Otherwise fall through to the handling below to get an error emitted.
    }

    TokenKind next = peek().kind;
    Token identifier;
    if (next == TokenKind::Identifier) {
        identifier = consume();
    }
    else if (next != TokenKind::Dot && next != TokenKind::DoubleColon &&
             (options & NameOptions::ExpectingExpression)) {
        if (!haveDiagAtCurrentLoc())
            addDiag(diag::ExpectedExpression, peek().location());
        identifier = Token::createMissing(alloc, TokenKind::Identifier, peek().location());
    }
    else {
        identifier = expect(TokenKind::Identifier);
    }

    switch (peek().kind) {
        case TokenKind::Hash: {
            auto parameterValues = parseParameterValueAssignment();
            ASSERT(parameterValues);
            return factory.className(identifier, *parameterValues);
        }
        case TokenKind::OpenBracket: {
            uint32_t index = 1;
            scanTypePart<isSemicolon>(index, TokenKind::OpenBracket, TokenKind::CloseBracket);
            if ((options & NameOptions::ForeachName) == 0 ||
                peek(index).kind != TokenKind::CloseParenthesis) {

                SmallVectorSized<ElementSelectSyntax*, 4> buffer;
                do {
                    buffer.append(&parseElementSelect());
                } while (peek(TokenKind::OpenBracket));

                return factory.identifierSelectName(identifier, buffer.copy(alloc));
            }
            else {
                return factory.identifierName(identifier);
            }
        }
        default:
            return factory.identifierName(identifier);
    }
}

ParameterValueAssignmentSyntax* Parser::parseParameterValueAssignment() {
    if (!peek(TokenKind::Hash))
        return nullptr;

    auto hash = consume();
    return &factory.parameterValueAssignment(hash, parseArgumentList(/* isParamAssignment */ true));
}

ArgumentListSyntax& Parser::parseArgumentList(bool isParamAssignment) {
    Token openParen;
    Token closeParen;
    span<TokenOrSyntax> list;

    auto allowEmpty = isParamAssignment ? AllowEmpty::False : AllowEmpty::True;

    parseList<isPossibleArgument, isEndOfParenList>(
        TokenKind::OpenParenthesis, TokenKind::CloseParenthesis, TokenKind::Comma, openParen, list,
        closeParen, RequireItems::False, diag::ExpectedArgument,
        [this, isParamAssignment] { return &parseArgument(isParamAssignment); }, allowEmpty);

    return factory.argumentList(openParen, list, closeParen);
}

ArgumentSyntax& Parser::parseArgument(bool isParamAssignment) {
    // check for empty arguments
    if (!isParamAssignment && (peek(TokenKind::Comma) || peek(TokenKind::CloseParenthesis)))
        return factory.emptyArgument(placeholderToken());

    // check for named arguments
    if (peek(TokenKind::Dot)) {
        auto dot = consume();
        auto name = expect(TokenKind::Identifier);

        auto [innerOpenParen, innerCloseParen, expr] = parseGroupOrSkip(
            TokenKind::OpenParenthesis, TokenKind::CloseParenthesis, [this, isParamAssignment]() {
                return isParamAssignment ? &parseMinTypMaxExpression() : &parseExpression();
            });

        return factory.namedArgument(dot, name, innerOpenParen, expr, innerCloseParen);
    }

    return factory.orderedArgument(isParamAssignment ? parseMinTypMaxExpression()
                                                     : parseExpression());
}

class PatternParser {
    Parser& parser;

public:
    PatternParser(Parser& parser) : parser(parser) {}

    template<typename TargetTy, typename ItemTy>
    TokenOrSyntax transformItem(ItemTy item) {
        if constexpr (std::is_same_v<ItemTy, Token> || std::is_same_v<TargetTy, ExpressionSyntax> ||
                      std::is_same_v<TargetTy, AssignmentPatternItemSyntax>) {
            return item;
        }
        else if constexpr (std::is_same_v<TargetTy, NamedStructurePatternMemberSyntax>) {
            if (AssignmentPatternItemSyntax::isKind(item->kind)) {
                auto& pattern = item->template as<AssignmentPatternItemSyntax>();
                return &parser.factory.namedStructurePatternMember(
                    expectIdentifierOrDefault(*pattern.key), pattern.colon,
                    toPattern(*pattern.expr));
            }
            else {
                return item;
            }
        }
        else if constexpr (std::is_same_v<TargetTy, OrderedStructurePatternMemberSyntax>) {
            if (ExpressionSyntax::isKind(item->kind)) {
                auto& expr = item->template as<ExpressionSyntax>();
                auto& pattern = parser.factory.expressionPattern(expr);
                return &parser.factory.orderedStructurePatternMember(pattern);
            }
            else {
                return &parser.factory.orderedStructurePatternMember(
                    item->template as<PatternSyntax>());
            }
        }
        else {
            (void)item;
            return Token();
        }
    }

    template<typename T>
    span<TokenOrSyntax> transferBuffer(const SmallVector<TokenOrSyntax>& buffer) {
        std::size_t size = buffer.size();
        auto* ptr = reinterpret_cast<TokenOrSyntax*>(
            parser.alloc.allocate(sizeof(TokenOrSyntax) * size, alignof(TokenOrSyntax)));
        auto* ptrBase = ptr;
        for (const auto& member : buffer) {
            *(ptr++) =
                std::visit([&](auto item) -> TokenOrSyntax { return transformItem<T>(item); },
                           static_cast<const std::variant<Token, SyntaxNode*>&>(member));
        }
        return { ptrBase, size };
    }

    template<typename T>
    SyntaxNode& finishParseBracePatternOrExpression(Token openBrace,
                                                    SmallVector<TokenOrSyntax>& buffer,
                                                    Token closeBrace) {
        if constexpr (std::is_same_v<T, SimpleAssignmentPatternSyntax>) {
            return *parser.alloc.emplace<T>(openBrace, transferBuffer<ExpressionSyntax>(buffer),
                                            closeBrace);
        }
        else if constexpr (std::is_same_v<T, StructuredAssignmentPatternSyntax>) {
            return *parser.alloc.emplace<T>(
                openBrace, transferBuffer<AssignmentPatternItemSyntax>(buffer), closeBrace);
        }
        else if constexpr (std::is_same_v<T, OrderedStructurePatternSyntax>) {
            return *parser.alloc.emplace<OrderedStructurePatternSyntax>(
                openBrace, transferBuffer<OrderedStructurePatternMemberSyntax>(buffer), closeBrace);
        }
        else {
            return *parser.alloc.emplace<NamedStructurePatternSyntax>(
                openBrace, transferBuffer<NamedStructurePatternMemberSyntax>(buffer), closeBrace);
        }
    }

    template<typename PatternTy, typename AssignTy>
    SyntaxNode& finishParseBracePatternOrExpression(bool isPattern, Token openBrace,
                                                    SmallVector<TokenOrSyntax>& buffer,
                                                    Token closeBrace) {
        if (isPattern) {
            return finishParseBracePatternOrExpression<PatternTy>(openBrace, buffer, closeBrace);
        }
        else {
            return finishParseBracePatternOrExpression<AssignTy>(openBrace, buffer, closeBrace);
        }
    }

    /// @return true if the given token represents a possible pattern, expression, comma, or default
    /// keyword.
    static bool isPossiblePatternOrExpressionOrCommaOrDefault(TokenKind kind) {
        switch (kind) {
            case TokenKind::Comma:
            case TokenKind::DefaultKeyword:
            case TokenKind::Dot:
            case TokenKind::DotStar:
                return true;
            default:
                return SyntaxFacts::isPossibleExpression(kind);
        }
    }

    /// @return true if the given token represents a possible pattern, expression, or comma
    static bool isPossiblePatternOrExpressionOrComma(TokenKind kind) {
        switch (kind) {
            case TokenKind::Comma:
            case TokenKind::Dot:
            case TokenKind::DotStar:
                return true;
            default:
                return SyntaxFacts::isPossibleExpression(kind);
        }
    }

    SyntaxNode& parseNamedPatternItem(SyntaxNode* firstExpr, bool& isPattern) {
        if (!firstExpr) {
            if (parser.peek(TokenKind::DefaultKeyword)) {
                firstExpr = &parser.factory.literalExpression(
                    SyntaxKind::DefaultPatternKeyExpression, parser.consume());
            }
            else {
                firstExpr = &parsePatternOrExpression(isPattern);
            }
        }
        auto colon = parser.expect(TokenKind::Colon);
        auto& secondExpr = parsePatternOrExpression(isPattern);
        if (isPattern) {
            return parser.factory.namedStructurePatternMember(expectIdentifierOrDefault(*firstExpr),
                                                              colon, toPattern(secondExpr));
        }
        else {
            return parser.factory.assignmentPatternItem(expectExpression(*firstExpr), colon,
                                                        expectExpression(secondExpr));
        }
    }

    /// note: parse a constant_expression or pattern-matching patrtern that starts with "'{", with
    /// priority given to ExpressionSyntax when ambiguous. isPattern is set to true when a
    /// PatternSytnax is returned, and not touched otherwise
    SyntaxNode& parseBracePatternOrExpression(bool& isPattern_) {
        bool isPattern = false;
        auto guard = ScopeGuard([&] { isPattern_ = isPattern_ || isPattern; });

        auto openBrace = parser.expect(TokenKind::ApostropheOpenBrace);
        auto token = parser.peek();
        SyntaxNode* firstExpr = nullptr;
        if (token.kind == TokenKind::DefaultKeyword) {
            // note: default-keyword not allowd in the BNF of LRM, we support it as an extension
            firstExpr = &parser.factory.literalExpression(SyntaxKind::DefaultPatternKeyExpression,
                                                          parser.consume());
        }
        else if (token.kind == TokenKind::CloseBrace) {
            // This is an empty pattern -- we'll just warn and continue on.
            parser.addDiag(diag::EmptyPatternMatchingPattern, openBrace.location());
            return parser.factory.orderedStructurePattern(openBrace, span<TokenOrSyntax>{},
                                                          parser.consume());
        }
        else {
            firstExpr = &parsePatternOrExpression(isPattern);
        }

        Token closeBrace;
        SmallVectorSized<TokenOrSyntax, 8> buffer;
        token = parser.peek();

        switch (token.kind) {
            case TokenKind::Colon: {
                auto colon = parser.consume();
                auto& secondExpression = parsePatternOrExpression(isPattern);
                if (isPattern) {
                    buffer.append(&parser.factory.namedStructurePatternMember(
                        expectIdentifierOrDefault(*firstExpr), colon, toPattern(secondExpression)));
                }
                else {
                    buffer.append(&parser.factory.assignmentPatternItem(
                        expectExpression(*firstExpr), colon, expectExpression(secondExpression)));
                }
                if (parser.peek(TokenKind::Comma)) {
                    buffer.append(parser.consume());
                    parser.parseList<isPossiblePatternOrExpressionOrCommaOrDefault,
                                     SyntaxFacts::isEndOfBracedList>(
                        buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace,
                        Parser::RequireItems::False, diag::ExpectedExpressionOrPattern,
                        [&] { return &parseNamedPatternItem(nullptr, isPattern); });
                }
                else {
                    closeBrace = parser.expect(TokenKind::CloseBrace);
                }
                return finishParseBracePatternOrExpression<NamedStructurePatternSyntax,
                                                           StructuredAssignmentPatternSyntax>(
                    isPattern, openBrace, buffer, closeBrace);
            }
            case TokenKind::OpenBrace: {
                auto innerOpenBrace = parser.consume();
                parser.parseList<isPossiblePatternOrExpressionOrComma,
                                 SyntaxFacts::isEndOfBracedList>(
                    buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace,
                    Parser::RequireItems::True, diag::ExpectedExpressionOrPattern,
                    [&] { return &parsePatternOrExpression(isPattern); });
                return parser.factory.replicatedAssignmentPattern(
                    openBrace, firstExpr->as<ExpressionSyntax>(), innerOpenBrace,
                    transferBuffer<ExpressionSyntax>(buffer), closeBrace,
                    parser.expect(TokenKind::CloseBrace));
            }
            case TokenKind::Comma: {
                // verification of pattern members is delayed to semantics-analysis
                buffer.append(firstExpr);
                buffer.append(parser.consume());
                parser.parseList<isPossiblePatternOrExpressionOrComma,
                                 SyntaxFacts::isEndOfBracedList>(
                    buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace,
                    Parser::RequireItems::True, diag::ExpectedExpressionOrPattern,
                    [&] { return &parsePatternOrExpression(isPattern); });
                return finishParseBracePatternOrExpression<OrderedStructurePatternSyntax,
                                                           SimpleAssignmentPatternSyntax>(
                    isPattern, openBrace, buffer, closeBrace);
            }
            case TokenKind::CloseBrace: {
                buffer.append(firstExpr);
                closeBrace = parser.consume();
                return finishParseBracePatternOrExpression<OrderedStructurePatternSyntax,
                                                           SimpleAssignmentPatternSyntax>(
                    isPattern, openBrace, buffer, closeBrace);
            }
            default: {
                // This is an error case; let the list handling code get us out of it.
                buffer.append(firstExpr);
                buffer.append(parser.expect(TokenKind::Comma));
                parser.parseList<isPossiblePatternOrExpressionOrComma,
                                 SyntaxFacts::isEndOfBracedList>(
                    buffer, TokenKind::CloseBrace, TokenKind::Comma, closeBrace,
                    Parser::RequireItems::False, diag::ExpectedExpressionOrPattern,
                    [&] { return &parsePatternOrExpression(isPattern); });
                return finishParseBracePatternOrExpression<OrderedStructurePatternSyntax,
                                                           SimpleAssignmentPatternSyntax>(
                    isPattern, openBrace, buffer, closeBrace);
            }
        }
    }
    /// note: this function only returns a ExpressionSyntax or PatternSyntax, with priority given to
    /// ExpressionSyntax when ambiguous. isPattern is set to true when a PatternSytnax is returned,
    /// and not touched otherwise
    SyntaxNode& parsePatternOrExpression(bool& isPattern) {
        switch (parser.peek().kind) {
            case TokenKind::DotStar:
                isPattern = true;
                return parser.factory.wildcardPattern(parser.consume());
            case TokenKind::Dot: {
                isPattern = true;
                auto dot = parser.consume();
                return parser.factory.variablePattern(dot, parser.expect(TokenKind::Identifier));
            }
            case TokenKind::TaggedKeyword: {
                // note: tagged union expression could not be constant_expression
                // fixme: maybe allow constant tagged union expression?
                isPattern = true;
                auto tagged = parser.consume();
                auto name = parser.expect(TokenKind::Identifier);
                // TODO: optional trailing pattern
                return parser.factory.taggedPattern(tagged, name, nullptr);
            }
            case TokenKind::ApostropheOpenBrace:
                return parseBracePatternOrExpression(isPattern);
            case TokenKind::OpenParenthesis: {
                // note: braced-pattern is not included in the BNF of LRM, but
                // examples in LRM won't complile without braced-pattern
                auto openParen = parser.consume();
                auto& sub = parsePatternOrExpression(isPattern);
                auto closeParen = parser.expect(TokenKind::CloseParenthesis);
                if (isPattern) {
                    return parser.factory.parenthesizedPattern(openParen, sub.as<PatternSyntax>(),
                                                        closeParen);
                }
                else {
                    return parser.factory.parenthesizedExpression(
                        openParen, sub.as<ExpressionSyntax>(), closeParen);
                }
            }
            default:
                return parser.parseSubExpression(Parser::ExpressionOptions::PatternContext, 0);
        }
    }
    // note: the input syntax should be of ExpressionSyntax or PatternSytnax
    Token expectIdentifierOrDefault(SyntaxNode& syntax) {
        if (syntax.kind == SyntaxKind::DefaultPatternKeyExpression) {
            return syntax.as<LiteralExpressionSyntax>().literal;
        }
        else if (IdentifierNameSyntax::isKind(syntax.kind)) {
            return syntax.as<IdentifierNameSyntax>().identifier;
        }
        else {
            parser.addDiag(diag::ExpectedIdentifier, syntax.sourceRange().start());
            return Token();
        }
    }
    // note: the input syntax should be of ExpressionSyntax or PatternSytnax
    ExpressionSyntax& expectExpression(SyntaxNode& syntax) {
        if (!ExpressionSyntax::isKind(syntax.kind)) {
            parser.addDiag(diag::ExpectedExpression, syntax.sourceRange().start());
        }
        return syntax.as<ExpressionSyntax>();
    }
    // note: the input syntax should be of ExpressionSyntax or PatternSyntax
    PatternSyntax& toPattern(SyntaxNode& syntax) {
        if (PatternSyntax::isKind(syntax.kind)) {
            return syntax.as<PatternSyntax>();
        }
        else {
            ASSERT(ExpressionSyntax::isKind(syntax.kind));
            return parser.factory.expressionPattern(syntax.as<ExpressionSyntax>());
        }
    }
};

PatternSyntax& Parser::parsePattern() {
    bool isPattern = false;
    PatternParser pparser(*this);
    auto& syntax = pparser.parsePatternOrExpression(isPattern);
    if (!isPattern) {
        return pparser.toPattern(syntax);
    }
    else {
        return syntax.as<PatternSyntax>();
    }
}

ConditionalPredicateSyntax& Parser::parseConditionalPredicate(ExpressionSyntax& first,
                                                              TokenKind endKind, Token& end) {
    SmallVectorSized<TokenOrSyntax, 4> buffer;

    MatchesClauseSyntax* matchesClause = nullptr;
    if (peek(TokenKind::MatchesKeyword)) {
        auto matches = consume();
        matchesClause = &factory.matchesClause(matches, parsePattern());
    }

    buffer.append(&factory.conditionalPattern(first, matchesClause));

    RequireItems require = RequireItems::False;
    if (peek(TokenKind::TripleAnd)) {
        buffer.append(consume());
        require = RequireItems::True;
    }

    parseList<isPossibleExpressionOrTripleAnd, isEndOfConditionalPredicate>(
        buffer, endKind, TokenKind::TripleAnd, end, require, diag::ExpectedConditionalPattern,
        [this] { return &parseConditionalPattern(); });

    return factory.conditionalPredicate(buffer.copy(alloc));
}

ConditionalPatternSyntax& Parser::parseConditionalPattern() {
    auto& expr = parseSubExpression(ExpressionOptions::PatternContext, 0);

    MatchesClauseSyntax* matchesClause = nullptr;
    if (peek(TokenKind::MatchesKeyword)) {
        auto matches = consume();
        matchesClause = &factory.matchesClause(matches, parsePattern());
    }

    return factory.conditionalPattern(expr, matchesClause);
}

EventExpressionSyntax& Parser::parseEventExpression() {
    EventExpressionSyntax* left;
    auto kind = peek().kind;
    if (kind == TokenKind::OpenParenthesis) {
        auto openParen = consume();
        auto& expr = parseEventExpression();
        auto closeParen = expect(TokenKind::CloseParenthesis);
        left = &factory.parenthesizedEventExpression(openParen, expr, closeParen);
    }
    else {
        Token edge;
        if (kind == TokenKind::PosEdgeKeyword || kind == TokenKind::NegEdgeKeyword ||
            kind == TokenKind::EdgeKeyword) {
            edge = consume();
        }

        auto& expr = parseSubExpression(ExpressionOptions::EventExpressionContext, 0);

        IffEventClauseSyntax* iffClause = nullptr;
        if (peek(TokenKind::IffKeyword)) {
            auto iff = consume();
            auto& iffExpr = parseSubExpression(ExpressionOptions::EventExpressionContext, 0);
            iffClause = &factory.iffEventClause(iff, iffExpr);
        }

        left = &factory.signalEventExpression(edge, expr, iffClause);
    }

    kind = peek().kind;
    if (kind == TokenKind::Comma || kind == TokenKind::OrKeyword) {
        auto op = consume();
        left = &factory.binaryEventExpression(*left, op, parseEventExpression());
    }
    return *left;
}

ExpressionSyntax& Parser::parseNewExpression(NameSyntax& newKeyword,
                                             bitmask<ExpressionOptions> options) {
    // If we see an open bracket, this is a dynamic array new expression.
    auto kind = peek().kind;
    if (kind == TokenKind::OpenBracket) {
        auto openBracket = consume();
        auto& sizeExpr = parseExpression();
        auto closeBracket = expect(TokenKind::CloseBracket);

        ParenthesizedExpressionSyntax* initializer = nullptr;
        if (peek(TokenKind::OpenParenthesis)) {
            auto openParen = consume();
            auto& initializerExpr = parseExpression();
            initializer = &factory.parenthesizedExpression(openParen, initializerExpr,
                                                           expect(TokenKind::CloseParenthesis));
        }
        return factory.newArrayExpression(newKeyword, openBracket, sizeExpr, closeBracket,
                                          initializer);
    }

    // Enforce rules for super.new placement.
    if (newKeyword.kind == SyntaxKind::ScopedName) {
        auto& scoped = newKeyword.as<ScopedNameSyntax>();
        if (scoped.right->kind == SyntaxKind::ConstructorName &&
            scoped.left->getLastToken().kind == TokenKind::SuperKeyword) {
            if ((options & ExpressionOptions::AllowSuperNewCall) == 0) {
                addDiag(diag::InvalidSuperNew, scoped.right->getFirstToken().location())
                    << newKeyword.sourceRange();
            }
        }
    }

    // Otherwise this is a new-class or copy-class expression.
    // new-class has an optional argument list, copy-class has a required expression.
    // An open paren here would be ambiguous between an arg list and a parenthesized
    // expression -- we resolve by always taking the arg list.
    if (kind == TokenKind::OpenParenthesis)
        return factory.newClassExpression(newKeyword, &parseArgumentList());

    if (isPossibleExpression(kind)) {
        if (newKeyword.kind != SyntaxKind::ConstructorName)
            addDiag(diag::ScopedClassCopy, peek().location()) << newKeyword.sourceRange();
        return factory.copyClassExpression(newKeyword, parseExpression());
    }

    return factory.newClassExpression(newKeyword, nullptr);
}

TimingControlSyntax* Parser::parseTimingControl(bool isSequenceExpr) {
    switch (peek().kind) {
        case TokenKind::Hash:
        case TokenKind::DoubleHash: {
            auto hash = consume();
            ExpressionSyntax* delay;
            if (hash.kind == TokenKind::DoubleHash && peek(TokenKind::OpenBracket)) {
                if (peek(1).kind == TokenKind::Star || peek(1).kind == TokenKind::Plus) {
                    Token openBracket = consume();
                    Token op = consume();
                    return &factory.shortcutCycleDelayRange(hash, openBracket, op,
                                                            expect(TokenKind::CloseBracket));
                }
                else {
                    delay = &parseOpenRangeElement();
                }
            }
            else {
                delay = &parsePrimaryExpression(!isSequenceExpr);
            }

            SyntaxKind kind =
                hash.kind == TokenKind::Hash ? SyntaxKind::DelayControl : SyntaxKind::CycleDelay;
            return &factory.delay(kind, hash, *delay);
        }
        case TokenKind::At: {
            auto at = consume();
            switch (peek().kind) {
                case TokenKind::OpenParenthesis: {
                    auto openParen = consume();
                    if (peek(TokenKind::Star)) {
                        auto star = consume();
                        return &factory.implicitEventControl(at, openParen, star,
                                                             expect(TokenKind::CloseParenthesis));
                    }

                    auto& eventExpr = parseEventExpression();
                    auto closeParen = expect(TokenKind::CloseParenthesis);
                    return &factory.eventControlWithExpression(
                        at, factory.parenthesizedEventExpression(openParen, eventExpr, closeParen));
                }
                case TokenKind::OpenParenthesisStar: {
                    // Special case since @(*) will be lexed as '@' '(*' ')'
                    auto openParen = consume();
                    return &factory.implicitEventControl(at, openParen, Token(),
                                                         expect(TokenKind::CloseParenthesis));
                }
                case TokenKind::Star:
                    return &factory.implicitEventControl(at, Token(), consume(), Token());
                default:
                    return &factory.eventControl(at, parseName());
            }
        }
        case TokenKind::RepeatKeyword: {
            auto repeat = consume();
            auto openParen = expect(TokenKind::OpenParenthesis);
            auto& expr = parseExpression();
            auto closeParen = expect(TokenKind::CloseParenthesis);
            return &factory.repeatedEventControl(repeat, openParen, expr, closeParen,
                                                 parseTimingControl(/* isSequenceExpr */ false));
        }
        default:
            return nullptr;
    }
}

ExpressionSyntax& Parser::parseArrayOrRandomizeMethod(ExpressionSyntax& expr) {
    auto with = consume();

    ParenExpressionListSyntax* args = nullptr;
    if (peek(TokenKind::OpenParenthesis)) {
        Token openParen, closeParen;
        span<TokenOrSyntax> items;
        parseList<isPossibleExpressionOrComma, isEndOfParenList>(
            TokenKind::OpenParenthesis, TokenKind::CloseParenthesis, TokenKind::Comma, openParen,
            items, closeParen, RequireItems::False, diag::ExpectedExpression,
            [this] { return &parseExpression(); });

        args = &factory.parenExpressionList(openParen, items, closeParen);
    }

    ConstraintBlockSyntax* constraints = nullptr;
    if (peek(TokenKind::OpenBrace))
        constraints = &parseConstraintBlock(/* isTopLevel */ true);

    return factory.arrayOrRandomizeMethodExpression(expr, with, args, constraints);
}

bool Parser::isConditionalExpression() {
    uint32_t index = 1;
    while (true) {
        TokenKind kind = peek(index++).kind;
        switch (kind) {
            case TokenKind::Question:
                return true;
            case TokenKind::CloseParenthesis:
                return false;
            case TokenKind::OpenParenthesis:
                if (!scanTypePart<isNotInType>(index, TokenKind::OpenParenthesis,
                                               TokenKind::CloseParenthesis)) {
                    return false;
                }
                break;
            case TokenKind::OpenBrace:
                if (!scanTypePart<isNotInType>(index, TokenKind::OpenBrace,
                                               TokenKind::CloseBrace)) {
                    return false;
                }
                break;
            case TokenKind::OpenBracket:
                if (!scanTypePart<isNotInType>(index, TokenKind::OpenBracket,
                                               TokenKind::CloseBracket)) {
                    return false;
                }
                break;
            default:
                if (isNotInType(kind))
                    return false;
                break;
        }
    }
}

} // namespace slang
