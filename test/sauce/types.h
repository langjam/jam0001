#pragma once

#include <AK/HashMap.h>
#include <AK/OwnPtr.h>
#include <AK/String.h>
#include <AK/Variant.h>
#include <AK/Vector.h>
#include <typeinfo>

struct Type {
    // ...
};

struct CommentResolutionSet;
struct Context;
struct FunctionNode;

struct Value;
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
    Variant<Empty, int, String, Type*, FunctionValue, NonnullRefPtr<CommentResolutionSet>, NativeFunctionType> value;
};

struct CommentResolutionSet : public AK::RefCounted<CommentResolutionSet> {
    Vector<Value> values;
};

struct Context {
    Vector<HashMap<String, Value>> scope;
    Vector<HashMap<Comment*, Vector<Value>>> comment_scope;
    Vector<Comment*> unassigned_comments;
};
