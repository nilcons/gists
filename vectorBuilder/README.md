Semantically correct `vectorBuilder` for Conduit
================================================

I've recently read Michael Snoyman's blog post on building vectors
efficiently for streaming chunked data:
https://www.fpcomplete.com/blog/2014/07/vectorbuilder-packed-conduit-yielding

It neatly solves an important problem, but it's not perfect. To avoid
the costly transformer stack overhead, the `vectorBuilder` combinator
does everything on the level of `IO` (or `ST`, the other monad to
manipulate mutable vectors). But this means that it cannot `yield` the
created vector as soon as it fills one, but have to delay until the
user code requests more data (and then yields all the created vectors
at once).

This is hacky and it breaks the nice streaming semantics of
Conduit. The user has to be aware of this and work-around
it… All-in-all, it's ugly. So, can we fix it? I wondered…

## Angle of attack

The problem is that we cannot "escape" from the middle of an `IO`
computation to go back to the level of `ConduitM` to yield the finished
chunk. One possible fix then is _not_ to go down _all the way_ to
`IO`. But to work in an extended IO, where we can temporarily escape
the computation with a value and then continue from where we left
off. ("Hmm, I am going to name the resulting abstraction
Exception–Continuation monad", I thought at this point.)

But remember, the point of going down to `IO` was that transformers
are costly. So I wanted to know how costly are they. How much will we
have to pay?

## Reality check

I decided to do some benchmarking: to measure a basically pure `IO`
computation "lifted" in different transformers. We are talking about
comparing the following two functions:

```haskell
direct :: IOVector Int -> IOVector Int -> IO ()
direct v1 v2 =
  forM_ [0..n-1] $ \i -> do
    x <- MV.unsafeRead v1 i
    MV.unsafeWrite v2 (2*i) x
    MV.unsafeWrite v2 (2*i + 1) x

transformed :: MonadIO m => IOVector Int -> IOVector Int -> m ()
transformed v1 v2 =
  forM_ [0..n-1] $ \i -> do
    x <- liftIO $ MV.unsafeRead v1 i
    liftIO $ MV.unsafeWrite v2 (2*i) x
    liftIO $ MV.unsafeWrite v2 (2*i + 1) x
```

What's important here is that we are doing _very cheap_ IO operations
(poking in mutable vectors), which means if there's any overhead,
we'll see it clearly. (Also, I chose an example similar to Michael's:
duplicating elements from one vector into another). Note that we have
3 `liftIO`s inside the `transformed` loop, which we could have
combined into one. This is on purpose; but what's really important is
that the `liftIO`s are _inside_ the loop, not outside.

And the benchmark:

```haskell
n = 200000

benchmarks :: IOVector Int -> IOVector Int -> [Benchmark]
benchmarks v1 v2 = [
  bench "direct" $ whnfIO $ direct v1 v2,
  bench "identity" $ whnfIO $ runIdentityT $ transformed v1 v2,
  bench "maybe" $ whnfIO $ fmap fromJust $ runMaybeT $ transformed v1 v2,
  bench "cont" $ whnfIO $ flip runContT return $ transformed v1 v2
  ]

main :: IO ()
main = do
  v1 <- MV.replicate n 5
  v2 <- MV.replicate (2*n) 0
  defaultMain $ benchmarks v1 v2
```

* I expected `IdentityT IO` be exactly like `IO`, and it turned out to
    be right. (Well, mostly... See later.)
* I expected `MaybeT IO` to be slower than `IO`, but I didn't know by
    how much. I was surprised to see that it incurs only a very little
    overhead. (Around 10%)
* I also added `ContT`, because I'm sneaky and I knew what the
  solution will be before getting to the end of the book. ;) And I was
  really happy to see that it incurred no overhead at all! Hooray,
  that means that we have a chance!

I also benchmarked a few other monad transformers (you can see the
complete code in [transformedIO.hs](./transformedIO.hs)), in particular `Proxy` (pipes
library) and `ConduitM`. We know that the overhead there cannot be
eliminated, but I was interested in just how large it is. Well, it's
between 10–200 times of a slow-down. (And there is a significant
difference between the two libraries, which is weird, as the internal
structure is quite similar. At least from the standpoint of this
measurement, as we are not touching any pipes or conduit
functionality. Maybe this is worth investigating.)

**Warning!** An important note: don't consider this "result" in any way
seriously. The whole thing is _extremely_ sensitive to the exact code
you are measuring! For example, if you replace `forM_ [0..n-1]` with
`V.forM_ v`, where `v` is a vector with values `0..n-1`, then the
above stops being true. And every transformer (except `IdentityT`)
incurs at least 6× penalty!

Also, playing with the code and rearranging it in different ways I
once created a version where the transformed variants (with
`IdentityT`, `MaybeT` and `ContT`) where significantly _faster_ than
the basic `IO` version. Optimization is tricky, benchmarking is
tricky, so again: don't take my results at face value. If you need to
know how a transformer will affect your code — measure your code.

## Exception–Continuation monad transformer

OK, now that we think that we have a chance to do this, how should
this "Exception–Continuation" thingy look like? Well, it should look
exactly like a minimal conduit or pipe that can only yield. But,
written in some sort of continuation passing style.

It should be a topic for another (series) of posts how to come up and
work with these, but here's what I used:

```haskell
newtype ECT o m a =
  EC { unEC :: forall r. (a -> m r) -> (o -> m r -> m r) -> m r }
```

This very closely corresponds to the much more easy to understand:

```haskell
newtype ECConcreteT o m a = ECConcrete { runECConcrete :: m (ECStep o m a) }

data ECStep o m a = ECPure a
                  | ECYield o (ECConcreteT o m a)
```

But, the "concrete" version is useless for us, as it would incur the
same kind of overhead as conduits or pipes.

Look into the [ECT.hs](./ECT.hs) for details. (Only the first part is
needed for the actual solution)

## Implementing the `vectorBuilder`

Modifying Michael's `vectorBuilder` implementation was relatively
straight-forward. It even gets simplified a bit, because we don't need
the logic for collecting the finished chunks. You can compare
[my code](./VectorBuilder.hs) to
[Michael's](http://hackage.haskell.org/package/conduit-combinators-0.2.8.2/docs/src/Data-Conduit-Combinators.html#vectorBuilder).
The only tricky part is in `liftInnerYields`, which has morphed from
Michael's `onAwait`.  (But this again, belong more to the topic of
understanding how to transform regular code to the inverted
continuation-passing/Codensity style code.)

So finally, what's the result? How does it compare to the original
"raw" `IO`-based `vectorBuilder`? It's actually faster! I don't really
know why. It might gain a bit of an advantage from the slightly
simplified logic, but not much. Anyway, the speed-up is not very big,
about 12%.

## Conclusions

A definitive, though cautious, YAY!

The reason for caution is that the measurement is only for one, very
simple test case. (I just used the example from Michael's blog post.)
And referring back, this is a tricky business. It still remains to be
seen how it fares in a more complicated real-life example. (Anyone?)

I'm also not very satisfied with the type signature of my
`vectorBuilder`.

1. It is less general than the original version. I am still thinking
   if anything can be done about it. (But, note that the _use_ didn't
   chnage at all: compare `rechunk3` and `rechunk4` in
   [rechunk.hs](./rechunk.hs).)
2. The `ECT` appears in the API. There's nothing to be done about it
   (this _is_ the solution, after all), but it needs to be properly
   explained for the users. Anyone knows if this already exists
   somewhere? (Oh, and maybe this can be done with the regular
   `ContT`. I have to think about it...)

I think, `vectorBuilder` combinator is a great addition to the Conduit
family, and with the proper semantics it will be even greater!
