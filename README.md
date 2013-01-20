Falling
=======

**Falling** is a set of purely functional generic constructs to build a Purely Functional Physics Engine. It is born from three observations:

  * 2d and 3d Physics Engines work basically the same way. Thus, there is no reason to write the same, boring, bookkeeping code twice. More than that, all the collision detection and collision responces should be writable independently from the dimension used (provided basic operators like dot products, matrix multiplication, etc. are specialized for each dimensions);
  * Most physics engine are, somehow, *monolitic*. It is always hard to plug in our own algorithms, or our own custom rigid-body types on an existing physics engine. For example, its is hard to add hyper-realistic aerodynamic features on a Rigid Body based Physics Engine. We try to solve this problem: for example, one can plug its own integration methods and its own constraint solver without loosing the collision detection and collision graph stuffs.
  * A Physics Engine has no reasons to be stateful. Thus, we chose to make it pure.

### Genericity wrt dimmension
**Falling** can easily addapted to any useful dimension, simply by feeding its generic types with custom, specialized geometric primitives. Thus, defining how a transform in 2d works, how you compute an inertia momentum in 2d, and which primitives you use for rigid bodies shapes, you can have a fully working 2d Physics Engine: **Falling** takes automatically care of all the bookkeeping, collision detection and collision response for you.

See **Falling2d** (https://github.com/sebcrozet/falling2d) and **Falling3d** (https://github.com/sebcrozet/falling3d) for examples.
### Genericity wrt rigid bodies
Even if **Falling** comes with code to handle rigid bodies, you can define your own bodies types and use this library for all the book-keeping work only (collision graph, islands construction, and integrate-detect-compute forces execution flow). In addition, according to the classes you shoose to implement, you might be able to re-use easily most of the existing code for contact resolution and collision detection.

# Compilation
**Falling** is not yet available on hackage, thus you will have to grab it from here.
We strongly recommend to use **cabal** for the compilation routine.

    $ git clone git@github.com:sebcrozet/falling.git
    $ cd falling
    $ cabal install
