export const words = {
    '+': (stack: any[]) => {
        stack.push(stack.pop() + stack.pop());
        return stack;
    },
    'dup': (stack: any[]) => {
        const top = stack.pop();
        stack.push(top);
        stack.push(top);
        return stack;
    },
};