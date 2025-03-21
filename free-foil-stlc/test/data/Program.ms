-- Statement order matters.
-- No mutability.
-- Shadowing is allowed.
-- The syntax of the program (e.g., in this file) and the body of a module is the same.

-- Type annotation.

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
      
      -- `#Lib.piLike` means access to `piLike` defined in the module `Lib`.
      -- `+` means addition.
      
      whereExpression = global + 5 + #Lib.piLike + #Local.local;
    };
    
  -- `*` means multiplication.
  
  piLike = #Lib.piLike + 10;
  
  
  shadow = 5;
  
  -- `export` the `shadow` definition and the type annotation if available.
  
  export shadow;
  
  shadow = 8;
  
  -- shadows the previous `export shadow`.
  -- `shadow = 8` is exported instead.
  
  export shadow;
  
  g = (6 + 7) + application;
  
  export application, piLike, g;
};

module World where {
  -- Import all statements from the module `Hello`.
  
  import * from Hello;
  
  -- The local `shadow` is bound to the `shadow` from `Hello`.
  
  shadow = shadow;
  
  -- Check that `shadow` has the type `Int`.
  
  #typecheck |- shadow <= Int;
  
  -- Import only `piLike` from the file.
  
  import { piLike } from "./Lib.stlc";
  
  t = \p . application + piLike + global + shadow + g + p;
};

#typecheck |- #World.t <= Int -> Int;

-- Synthesize the type for `t` and check that it's `Int -> Int`.
-- The mismatch should be reported somehow.

#typesynth |- #World.t => ! Int -> Int;

-- Synthesize the unknown type for `shadow` having `s : Int` in the context.
-- The found type should be reported somehow.

#typesynth s : Int |- #World.shadow + s => ? ;