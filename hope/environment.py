    
class Environment:
    def __init__(self, parent=None):
        self.bindings = {}
        self.parent = parent
        
    def put(self, name, value, depth=None):
        if depth is None:
            depth = name.binding_depth
            
        if depth == 0:
            self.bindings[name.name] = value
        else:
            self.parent.put(name, value, depth - 1)

    def get(self, name, depth=None):
        if depth is None:
            depth = name.binding_depth

        if depth == 0:
            if name.name in self.bindings:
                return self.bindings[name.name]
            return None

        return self.parent.get(name, depth - 1)

