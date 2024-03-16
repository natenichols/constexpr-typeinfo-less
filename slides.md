Standardized Constexpr Type-Ordering
====================================

Slides for P2839R2, EWG, Tokyo 2024
-----------------------------------

What are we proposing?
----------------------

```cpp
// <compare>
template <typename T, typename U>
constexpr std::strong_ordering type_order_v =
    /* implementation-defined; use the mangler, Luke! */;
```

- There are other syntax options, but they aren't as good.
- There are other semantic options, but they aren't as good.


Why do we need this?
--------------------

In short, sort + optionally unique.

Canoncial type set and type multiset.

Library authors "abuse" `__PRETTY_FUNCTION__` for this, to great (and occasionally wrong)
results.

It is a common need - and should be uniformly supported.


Why not just standardize a typeset?
-----------------------------------

Because sooner or later you need to make a tuple, variant, or some other
typelist out of it, and that translates to library ABI.

A canonical ordering is required.


Why can't reflection deal with this?
------------------------------------

Reflection isn't a standardized order; it would also lead to multiple
incompatible bad implementations.

We know there is a need. Let us fulfil it, portably.


Is "implementation-defined" really ok?
--------------------------------------

Yes. It seems the best of bad choices.

We tried a fully standardized order - it is very complicated. See annex in paper.
 
- The standard doesn't have the "vocabulary" to deal with uniformity between
  compilation units. **ABIs do**.
- ABIs change mangling for everything we standardize anyway
    - The template specialization grammar includes the expression grammar (!)
- Shackling the C++ specification to define orderings for everything when ABIs
  already do this work is rather pointless.
- For typesets, being consistent within a linkage universe is _enough_.
- We have been strongly encouraged by implementors to go this route.
    - it minimizes effort (they already have a mangling implementation, they don't need another)
    - **look at the itanium ABI bugtracker.** We do not need that pain in WG21.


Why `<compare>`
---------------

The only other natural place is `<type_traits>`, but why make `<type_traits>`
depend on `<compare>`?


Why not use `type_info::before`?
--------------------------------

- It ignores cv-ref qualifiers
- ABI break for all implementations

So, no.


Do you have wording?
--------------------

Yes.

It has not been reviewed by library and core experts, but I'll get that done.


Implementation concerns?
------------------------

This is inherently implementable - we have all the pieces.

Some implementors have expressed disdain at having to bring the mangler into the frontend.

**Note:** the mangler in the front-end and the one in the back-end need not agree. It's implementation defined. Let them do the sensible thing.


Why not just refer to the itanium ABI?
--------------------------------------

We could just say _do what itanium ABI does_. It *is* an ISO standard.

It seems like that is an implementation burden for compilers that do not have a
mangler implementation for the itanium ABI, though. Seems like letting
implementations do what they will makes more sense.


Alternatives to proposed semantics
----------------------------------

- Re-do everything that the itanium ABI mangling specification does.
    - yay, another specification to implement that can only ship bugfixes every 3 years.
- nothing. We let people roll their own bad orderings based on reflection. Ewww.

Alternatives to proposed syntax
-------------------------------

1. if we get universal template parameters, ship `entity_ordering_v` instead of
   `type_ordering_v`
2. use a reflection-based function `std::partial_order(std::meta::info, std::meta::info)`
    - it's a partial order, we're definitely not ordering expressions.
    - a partial order is decidedly less compatible with `std::sort`.
    - This is a good idea, but why not have the variable-template `strong_ordering` first?
3. heterogeneous `constexpr strong_ordering(std::type_identity<T>, type_identity<U>)`
    - not as discoverable
    - depends on `type_identity`
    - names in `std::` are not *that* precious.
    - compiles slower (two extra class template instantiations).




