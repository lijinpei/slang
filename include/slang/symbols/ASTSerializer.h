//------------------------------------------------------------------------------
//! @file ASTSerializer.h
//! @brief Support for serializing an AST
//
// File is under the MIT license; see LICENSE for details
//------------------------------------------------------------------------------
#pragma once

#include "slang/text/Json.h"
#include "slang/util/Util.h"

namespace slang {

class AttributeSymbol;
class Compilation;
class ConstantValue;
class Constraint;
class Expression;
class Statement;
class Symbol;
class Type;
class TimingControl;
class ConditionalPredicate;
class MatchedPattern;
class WildcardPattern;
class VariablePattern;
class ExpressionPattern;
class TaggedPattern;
class OrderedStructurePattern;
class NamedStructurePattern;

class ASTSerializer {
public:
    ASTSerializer(Compilation& compilation, JsonWriter& writer);

    void setIncludeAddresses(bool set) { includeAddrs = set; }

    void serialize(const Symbol& symbol);
    void serialize(const Expression& expr);
    void serialize(const Statement& statement);
    void serialize(const TimingControl& timing);
    void serialize(const Constraint& constraint);
    void serialize(std::string_view value);
    void serialize(const ConditionalPredicate& pred);
    void serialize(const MatchedPattern& pattern);

    void startArray(string_view name);
    void endArray();
    void startObject();
    void endObject();

    void write(string_view name, string_view value);
    void write(string_view name, int64_t value);
    void write(string_view name, uint64_t value);
    void write(string_view name, double value);
    void write(string_view name, bool value);
    void write(string_view name, const std::string& value);
    void write(string_view name, const Symbol& value);
    void write(string_view name, const ConstantValue& value);
    void write(string_view name, const Expression& value);
    void write(string_view name, const Statement& value);
    void write(string_view name, const TimingControl& value);
    void write(string_view name, const Constraint& value);
    void write(string_view name, const MatchedPattern& value);
    void write(string_view name, const ConditionalPredicate& value);

    void writeLink(string_view name, const Symbol& value);

    template<typename T, std::enable_if_t<std::is_integral_v<T> && std::is_signed_v<T>, int> = 0>
    void write(string_view name, T value) {
        write(name, int64_t(value));
    }

    template<typename T, std::enable_if_t<std::is_integral_v<T> && std::is_unsigned_v<T>, int> = 0>
    void write(string_view name, T value) {
        write(name, uint64_t(value));
    }

    template<typename T, std::enable_if_t<std::is_pointer_v<T>, int> = 0>
    void write(string_view name, T value) = delete;

    template<typename T>
    void write(string_view name, not_null<T> value) = delete;

private:
    friend Symbol;
    friend Expression;
    friend Statement;
    friend TimingControl;
    friend Constraint;
    friend MatchedPattern;
    friend WildcardPattern;
    friend VariablePattern;
    friend ExpressionPattern;
    friend TaggedPattern;
    friend OrderedStructurePattern;
    friend NamedStructurePattern;

    template<typename T>
    void visit(const T& symbol);

    void visit(const MatchedPattern&);
    void visit(const WildcardPattern&);
    void visit(const VariablePattern&);
    void visit(const ExpressionPattern&);
    void visit(const TaggedPattern&);
    void visit(const OrderedStructurePattern&);
    void visit(const NamedStructurePattern&);

    void visitInvalid(const Expression& expr);
    void visitInvalid(const Statement& statement);
    void visitInvalid(const TimingControl& timing);
    void visitInvalid(const Constraint& timing);

    Compilation& compilation;
    JsonWriter& writer;
    bool includeAddrs = true;
};

} // namespace slang
