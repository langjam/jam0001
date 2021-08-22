#pragma once

#include <AK/HashMap.h>
#include <AK/OwnPtr.h>
#include <AK/String.h>
#include <AK/Variant.h>
#include <AK/Vector.h>
#include <typeinfo>

enum class NativeType {
    Int,
    String,
    Any,
};

struct Type;
struct TypeName {
    String name;
    NonnullRefPtr<Type> type;
};

struct Type : public RefCounted<Type> {
    Type(Variant<Vector<TypeName>, NativeType> decl)
        : decl(move(decl))
    {
    }
    
    Variant<Vector<TypeName>, NativeType> decl;
};

struct CommentResolutionSet;
struct Context;
struct FunctionNode;

struct Value;
struct RecordValue {
    NonnullRefPtr<Type> type;
    Vector<Value> members;
};

struct NativeFunctionType {
    Value (*fn)(Context&, void*, size_t);
    Vector<String> comments;
};

struct Comment;

struct FunctionValue {
    NonnullRefPtr<FunctionNode> node;
    Vector<HashMap<String, Value>> scope;
    Vector<HashMap<Comment*, Vector<Value>>> comment_scope;
};

struct Value {
    Variant<Empty, int, String, NonnullRefPtr<Type>, FunctionValue, NonnullRefPtr<CommentResolutionSet>, NativeFunctionType, RecordValue> value;
};

struct CommentResolutionSet : public AK::RefCounted<CommentResolutionSet> {
    Vector<Value> values;
};

struct Context {
    Vector<HashMap<String, Value>> scope;
    Vector<HashMap<Comment*, Vector<Value>>> comment_scope;
    Vector<Comment*> unassigned_comments;
    size_t last_call_scope_start { 0 };
};

Value& flatten(Value& input);
