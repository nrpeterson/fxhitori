# FxHitori

This repo contains a Haskell library for building and leveraging solvers for the logic puzzle game Hitori.

I started it in December 2020, with a few purposes in mind:

1. Encode heuristics for solving puzzles in a way that is easily explained to a player.
2. Try to categorize puzzles based on which strategies are invoked and how often.
3. Write some code. :-) My day job doesn't let me do that much anymore...
4. Learn some Haskell. I had previously written some simple things in Haskell (basically solutions to Project Euler
   problems), but never anything serious.

This is still a work in progress; but, as of version 0.1.0, it contains several well-formed heuristics and can solve
boards considered 'easy'. (Next up is to add some heuristics to collapse the state space when we know that one cell or
another must be black.)

## What Still Needs Doing?

When I get around to it, a few projects on the horizon:

1. Writing tests!  I've just started reading about `quickcheck` and similar tools.
2. A UI! Could be curses or could be graphical, but I've got all the pieces to make great explanations of moves as they
   are being made.
3. Building out new heuristics.  I've got more in mind, but... vacation is over. :-)

## What Have I Learned?

I came into this extremely comfortable with functional programming; but, largely working in Scala rather than in
Haskell. So, this was less about learning functional programming from the ground up, and more about learning how Haskell
is written in practice and what tools are commonly leveraged.

### Monad Transformers

I was already comfortable with monads, but had not previously worked with monad transformer stacks. I initially built
things in explicit monad stacks using `transformers`, settling on
```haskell
MaybeT (WriterT [(Move, Explanation)] (State Board))
```
as my description for running a step of solving the game board: we build stateful functions (they take a board and 
return an updated one if there's a new move found), which record which moves have been taken and their explanations. 
This worked extremely well.

The biggest revelation for me -- and the one that made the rest of it all start to make sense -- was `MaybeT`. Moving
the `Maybe` into the monad layer meant not only that do-notation was simpler... but that I could represent solving the 
game as "just run this monad forever", and it would naturally stop the first time the internal value switched over to
`Nothing`.  Fantastic.

The downside to all of this, of course, was ease of use: that type is a real burden to carry around, for one thing; but
for another,  I found that having to determine the right number of `lift`s for a given operation was tedious, and that
the resulting code was really hard to follow because the `lift`s naturally took me pretty far out of context.

I was just in the process of writing my own wrappers when I decided to do some more searching and reading, and found...

### MTL

The ability MTL provides to abstract away the specifics of the monad stack and just focus on what capabilities it has 
made life *significantly* better. This allowed me to rewrite the `stepRunner` in `FxHitori.Strategy` to say "give me
any monad `m` that has the correct state and writer components, and I'll give you back a `MaybeT m`. This will be 
helpful later on when I go to build a UI (whether graphical or console-based), as including IO at the base of the stack
will "just work".

### Lenses

I took a rather deep dive into lenses and traversals to get here.  (Initially, I even implemented them myself... but
eventually the boilerplate of creating lenses for data objects got to me, and I relented.) I'm currently using the
excellent
[microlens-platform](https://hackage.haskell.org/package/microlens-platform) library for this purpose.

If you want to figure out why on earth a `Lens` isn't just a getter and setter, I strongly suggest the 
[Lens over Tea](https://artyom.me/lens-over-tea-1) series by Artyom Kazak (who also built the `microlens` libraries).

I do find that there is extreme risk in using `lens` or `microlens` of making your code a symbol soup, which is a pet
peeve of mine; if a reasonably informed engineer can't look at your code and parse it easily, then you're probably doing
something wrong.

So, I've mostly tried to stick with `over`, `view`, and `set` rather than the more typical operator notation. However, I
have slowly but surely found myself more comfortable leveraging a few of the operators -- particularly those that seem
expressive and actually simplify things.

### Haddock

I find Haddock to be a pretty clever tool for generating documentation -- the way it has integrated with the typesystem
makes for documentation that is extremely readable both as code and in the lovely HTML it generates.