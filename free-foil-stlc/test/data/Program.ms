-- Statement order matters.
-- Shadowing is allowed.
-- No mutability.
-- The syntax of the program (e.g., in this file) and the body of a module is the same.

-- Type annotation.
-- Must be provided before the definition.

global : Int;

-- Definition.

global = 3;

-- A module.

module Hello where {
  
};

-- Shadows the previous `Hello` module.

module Hello where {
  
  -- Create a module that contains all statements from the file.
  
  import * as Lib from "./Lib.stlc";
  
  -- Nested module.
  
  module Local where {
    local = global;
  };
  
  application : Int;
  
  -- Application of `whereFunction` to `whereExpression`.
  
  application = (whereFunction) whereExpression
    
    -- Like an anonymous module.
    
    where {
      whereFunction : Int -> Int;
      whereFunction = id;
      
      -- `#Lib.externalConst` means access to `externalConst` defined in the module `Lib`.
      -- `+` means addition.
      
      whereExpression = global + 5 + #Lib.externalConst + #Local.local;
    };
    
  -- `*` means multiplication.
  
  externalConst = #Lib.externalConst + 10;
  
  
  shadow = 5;
  
  -- Export (make visible from outside of the module) the `shadow` definition and its type annotation if available.
  
  export shadow;
  
  shadowRef = shadow;
  
  shadow = shadow + 8;
  
  -- Shadow the previous `export shadow`.
  -- The latter `shadow` is exported instead.
  
  export shadow;
  
  -- The `shadowRef` with the value `5` is exported.
  
  export shadowRef;
  
  g = (6 + 7) + application;
  
  export application, externalConst, g;
};

module World where {
  -- Import all statements from the module `Hello`.
  
  import * from Hello;
  
  -- The local `shadow` is bound to the `shadow` from `Hello`.
  
  shadow = shadow;
  
  -- Check that `shadow` has the type `Int`.
  
  #typecheck |- shadow <= Int;
  
  -- Import only `externalConst` from the file.
  
  import { externalConst } from "./Lib.stlc";
  
  t = \p. application + externalConst + global + shadow + g + p;
};

#typecheck |- #World.t <= Int -> Int;

-- Synthesize the type for `t` and check that it's `Int -> Int`.
-- The mismatch should be reported somehow.

#typesynth |- #World.t => ! Int -> Int;

-- Synthesize the unknown type for `shadow` having `s : Int` in the context.
-- The found type should be reported somehow.

#typesynth s : Int |- #World.shadow + s => ? ;