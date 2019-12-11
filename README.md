## golf - Game of Life (Functional-y)

Originally inspired by [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf),
then more recently by [Life is a Comonad](https://eli-jordan.github.io/2018/02/16/life-is-a-comonad/)
and [A Gentle Introduction to Comonads](https://www.youtube.com/watch?v=kVnJtiN1dbk) (video),
I wanted to try coding up my own version of
[Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) using comonads,
and this repo is the result. I wrote it without explicitly cheating with the video, but having just
watched it recently I remembered the data structure, and followed the types for the comonad
instance.

### Neighborhoods

Part of my interest was in seeing how the "evaluate neighborhood" step really worked. In my version
I've tried to use the comonad to re-focus on the neighbors and use the `extract`. The
[version from the video](https://scalafiddle.io/sf/IMX6BVK/26) actually used the grid structure
explicitly - I'd imagine that'd be more performant, but seems like somehow it knows too much.

### Wrapping

One of the questions I had in the neighborhood evaluation was also how to handle wrapping, or not.
It looks like you can bundle that whole difference up in the `extract` method of comonad (if you
allow the type parameter to be a monoid). I've coded up the wrapping version, which has no requirement
on the type parameter for the `PointedGrid`. However, if I allowed for the `T` parameter to be a
monoid, then I could have `extract` return `empty` if the `pt` was outside the `grid` coordinates.

### Miscellaneous

While I was pondering, having some idea that zippers were (a) probably another example of
comonads, (b) the derivative of a data structure, I started wondering if there was a relationship there.
I didn't really dig in too far, but [A Notation for Comonads](https://www.cs.kent.ac.uk/people/staff/dao7/publ/codo-notation-orchard-ifl12.pdf)
says "All container-like zippers are comonads", with a reference to
[When is a Container a Monad?](https://arxiv.org/pdf/1408.5809.pdf).
I'm glad to know there's folks out there that play with these ideas. I don't know when I'll
make the time to really understand, but it's still fun to play with.
