---
title: Standardized Constexpr Type Ordering
document: P2830R6
date: 2024-11-20
audience: EWG
author:
  - name: Nate Nichols
    email: <natenichols@cox.net>
  - name: Gašper Ažman
    email: <gasper.azman@gmail.com>
toc: true
---

# Abstract 

As of C++23, `std::type_info` provides a stable but _unspecified_ and
non-`constexpr` order on types.

This paper explores a standardized, constexpr ordering of types in C++.

This paper is split into two parts:

1. the design of the exposition-only function `TYPE-ORDER(x, y)`, which deals with
   how such an order should be defined,
2. how to expose this capability to the programmer (syntax).

# Revision History

0. New Paper
1. Revision 1
    - Introduce options to prevent changing `std::type_info::before`
    - Anonymous namespaces can't be empty
    - Add FAQ section
    - Add motivating examples
    - Add proposed syntax
    - Add appendix
2. Revision 2
    - Presented at the EWGi in Tokyo.
    - Propose to make the `TYPE-ORDER(X, Y)` definition `constexpr` and
      _implementation-defined_, as suggested by EWGi and BSI reviewers.
    - added wording
    - added more motivating examples
3. Revision 3
    - incorporates wording feedback from Jens Maurer, to be presented in an EWG
      telecon
4. Revision 4
    - incorporated feedback from the EWG discussion on May 17th
    - Added example of `__PRETTY_FUNCTION__` differences from Barry Revzin
      (thanks!)
    - Incorporated the fact that EWG decided that the syntax is reasonable,
      pending LEWG review.
    - Did a further pass on properly defining `SORT_KEY(x, y)`
5. Revision 5
    - did a lot more work on what we would need to standardize if we wanted to completely define the ordering.
    - presented to SG7 in Wroclaw, they said they want to fully define it
    - presented to EWG in Wroclaw, they forwarded with strong consensus to leave it implementation defined
    - wrote wording and relegated the sketch of the completely-defined order to a historical appendix
6. Revision 6
    - added feature-test macro
    
     
# Motivation

There is currently no way in portable C++ to sort types at compile-time.

Various libraries hack it together, mostly by utilizing `__PRETTY_FUNCTION__`,
but all such methods are non-portable and error-prone (consider
forward-declared enums - example in Annex). There are multiple stack-overflow
questions on the topic.

[One such implementation](https://github.com/libfn/functional)
is part of the monadic functional library by Bronek Kozicki et al.

Fundamentally, we need a way to canonicalize sets and multisets of types.
This is necessary when building any kind of compositional library in C++, from
`std::execution` [@P2300R7], mixin libraries, libraries of monadic
components and operators, policy-based libraries, etc.
The inability to sort and unique types leads to the instantiation of an
combinatorial number of templates instead of just one per typeset, which leads to
code-bloat and untenable compile-times.
The problem is fundamentally that without typeset canonicalization, we generate
different template specializations but equivalent behavior, and there is no way
to avoid this.

The goal here is to provide a _flexible_ language mechanism to let both
`Foo<A, B, C>` and `Foo<C, B, A>` produce the same underlying
`Foo_impl<A, B, C>`.

The reason we start with `TYPE-ORDER()` and not "just give me typesets" is that we
need flexibility; consider

- a given library might want to deduplicate on a part and keep either last or
    first, or even make it ill-formed:
    `Foo<pair<A, X>, pair<B, Y>, pair<A, Z>>`
    might want to be the same as
    `Foo_impl<pair<A, X>, pair<B, Y>>`
    or 
    `Foo_impl<pair<A, Z>, pair<B, Y>>`
- a given library might actually just want canonicalized multisets:
    `Foo<A, B, A, A, C>` should perhaps be `Foo<A, A, A, B, C>`
- or treat the first one as special:
    `Matrix<float, policy1, policy2, policy3>` should only deduplicate
    policies.

We must provide `TYPE-ORDER` in order to `sort` and `unique`; they are required
building blocks for any `set` primitive. Put another way, even if we
standardized a `set`, we'd need to somehow canonicalize the order (due to
mangling and debug info), leading us back here.

Without such canonicalization, it is infeasible to enumerate the set of
function templates to instantiate in a separate compilation unit. For the same
reason, it's utterly impossible to type-erase them in a fixed-size virtual
function table.


## Motivating Examples

This section introduces the kind of code we would like to write, regardless of
how this feature ends up being spelled. To not prejudice the reader in this
section, please assume the existence of an exposition-only `consteval` macro 
`TYPE-ORDER(x, y) -> std::strong_ordering` whose arguments can be any cv-ref
qualified types.

**Note:** while `TYPE-ORDER(x, y)` is defined on types, we can define a set of
class templates that take an arbitrary template argument, and `TYPE-ORDER`
those; this induces an order on all entities that can be template arguments.

Crucially, also consider the interactions with [@P1985R3] and [@P2841R1], which
introduce `concept` and `variable-template` arguments.


### Archetypal example: canonicalized typelist

(Kept short because further examples give concrete needs).

```cpp
struct A {};
struct B {};

// we want some way to enable
static_assert(std::is_same_v<typeset<A, B>, typeset<B, A, A>>);
```

Furthermore, we need the order to be the same in different compilation units.

### Canonicalizing policy-based libraries

Consider the needs of a library for type-erasure; we'd like the user to specify
the capabilities to erase. Fundamentally, these capabilities are a set.

Observe:

```cpp
using T1 = lib::dyn<ostreamable, json_serializable, iterable<int>, scalar_multiplicable<int>>;
```

Different users of the library will likely provide these capabilities in
different orders; this becomes especially problematic when writing functions:

```cpp
using T2 = lib::dyn<json_serializable, ostreamable, iterable<int>, scalar_multiplicable<int>>;
                    ~~~~~~ flipped ~~~~~~~~~~~~~~~
void f(T2 const&);
```

We can solve this by having type aliases, but if these sets are *computed*, we
are left without recourse:

```cpp
int sum = 0;
auto pipeline1 = 
   log(std::cerr) | sum_into(&sum) | imul(sum) | json_dump(std::cout);
// dyn<ostreamable, iterable<int>, scalar_multiplicable<int>, json_serializable>
auto pipeline2 = 
   sum_into(&sum) | imul(sum) | json_dump(std::cout) | log(std::cerr);
// dyn<iterable<int>, scalar_multiplicable<int>, json_serializable, ostreamable>
```

The above pipelines need the same type-erased interface for its input, and
neither is either `T1` or `T2`.


### Canonicalized variant

The most obvious example is a canonicalized `std::variant`, that is, something like

```cpp
template <typename... Ts>
using variant_for = apply_canonicalized<std::variant, Ts...>;
```

Building `apply_canonicalized` is not terribly difficult, as long as we have `TYPE-ORDER`.
Please see the appendices on how to do it.

It *would* be nice if `apply_canonicalized` was a language built-in, but to do that,
we need to first define `TYPE-ORDER(x, y)`. After we define `TYPE-ORDER`, putting
`apply_canonicalized` into `type_traits` is a far simpler proposition.

**Note:** `apply_canonicalized` is roughly `mp11::mp_sort` + `mp11::unique`
with the order derived from `TYPE-ORDER`.


Effectively any time a variant is type-indexed instead of integer-indexed, we
would prefer the compiler generate just one type per set, instead of
type-per-list.

Examples follow, but they are by no means exhaustive.


#### Multiple error kinds and `std::expected`

Consider a `std::expected` for an algorithm that can fail in several ways:

```cpp
auto read_packet(socket& sock)
    -> expected<message, variant<io_error, decode_error>>;
```

Such an algorithm doesn't care about whether the error type is
`variant<io_error, decode_error>` or `variant<decode_error, io_error>`. This is
not as much of a problem in cases where the programmer writes the types by
hand, but consider the generic situation where several errors must be
aggregated generically:

```cpp
template <typename Decoder>
auto read_packet(socket& sock)
    -> expected<Decoder::message_type,
                variant</*uniqued and sorted io_error + Decoder::error_types*/>>;
```

If we had member packs and pack aliases, and a nice built-in `set`, we could o the above as

```cpp
template <typename... Ts>
...using set = __builtin_uniqued_sorted<Ts...>;

variant<...set<io_error, Decoder::...error_types...>...>;
```


#### Suspension-point storage for asynchronous code

`std::execution` (and every other async library) needs a variant-type to store
the current result at a suspension point. In general, since it models a
"suspended" function call, it's analogous to some kind of

```cpp
variant<
    closure_tuple<set_value, Arg1_1, Arg1_2, Arg1_3>, // first possible entry point
    closure_tuple<set_value, Arg2_1>,                 // second possible entry point
    closure_tuple<set_error, ...>,
    ...
>
```

The venerable `rxcpp` library by Kirk Shoop calls this type a _notification_.

Consider the `transfer(scheduler)` algorithm; it receives all
`set_value(PACK)`, `set_error(PACK)` and possibly `set_stopped()` parameter
lists, _stores them in a_ `variant<tuple<CHANNEL, PACK>...>`, and resumes with
the values on a different scheduler once the compute resource becomes
available.

The code using it looks like this:

```cpp
some_computation | transfer(scheduler) | then([](auto&&...args){/*...*/})...
```

All the possible closure tuples are fundamentally unsorted, and generating the
assembly, debug information, and everything else for this type more than once
is a waste of compile-time and executable space (as well as instruction cache).

Furthermore, stamping out this type multiple times also duplicates the calling
code, and given the different indices, COMDAT folding cannot deduplicate those
code-paths.


#### Compositional State machine models

The states of most state-machines are fundamentally unordered, as are the
message types they receive. If a state machine is generated (such as in a
compile-time regex library), canonicalizing identically-behaving components
would lead to smaller state-machines and faster compile- and execution times.


### Canonicalized tuple

Similarly to a canonicalized `variant`, a `canonicalized_tuple` could be implemented as

```cpp
template <typename...Ts>
using canonical_tuple = apply_canonicalized<tuple, Ts...>;
// or
using canonical_tuple = tuple<...set<Ts...>...>;
```

This is far less useful on its own than the `variant`, since initializing that
type will be quite the chore. Instead, canonicalized tuples appear as a result
of the `environment` monads.

`std::execution` mixes in an environment, which consists at least of a
`stoppable_token`, a `scheduler`, and possibly other things like a `domain`,
and, at least in the authors' implementation, arbitrary other things. An
`allocator` is also optionally part of the environment.

The `environment<pair<Tags, Ts>...>` is a product type that is a map from `Tag`
to a value of type `T`, say `std::stop_token` to `never_stop_token{}` and
`scheduler` to `thread_pool_scheduler`.

One can push and pop things off the environment in different parts of the
pipeline; if the pushes come in the wrong order, the environment is difficult
to keep canonicalized.

```cpp
auto ss = std::stop_source();
auto tp = std::static_thread_pool(5);
auto env = empty_env{} 
    | push_env<stop_token>(ss.get_token())
    | push_env<scheduler_t>(tp.get_scheduler())
    | push_env<mylib::numa_domain_t>(2);
```

If you imagine those three calls to happen in different algorithm transformers,
it's quite likely they'll be in a different order, manifesting a different
type, which doesn't make much sense - the type behaves identically, this just
leads to code bloat.


### API stability

The authors have observed a necessity to sometimes split parts of such pipeline
compositions into different compilation units due to excessive compile times
and repetitive compilation.

This has proven difficult due to the brittle nature of type ordering of the
names of explicit template specializations involved. By far, the main culprit
has been the lack of a sensible canonicalization of names, as the order changes
far more frequently than the set of types.

Consider:

```
do_something()
    | error_if_odd()          // there is no semantic change
    | error_if_we_too_long()  // if we flip these lines
    | ...
```

Flipping the lines will flip the two exit error types in the error list,
however.

Having a defined, stable order of types proved invaluable (even if we did hack
the type order manually).


# Design discussion

The first two revisions of this proposal tried to construct a full type
ordering from first principles; EWGi then requested we change to an
implementation-defined order (use the mangler), but EWG subsequently reversed
that decision; SG7 affirmed this decision in Wroclaw, only to be reversed by
EWG again to implementation-defined.

This paper proposes the implementation-defined version.

## Desirable properties of `TYPE-ORDER(x, y)`

### Stability

The order should be the same across compilation units.
This is key for generating ABI-compatible vtables, for instance.

It also should be stable through time. An order based on an ABI-mangling scheme
satisfies this notion, at least.


### Free-standing

The order should be available on freestanding implementations. One crucial
use-case is replacing the exception mechanism on those platforms with return
values of `std::expected` or similar, and compromising that is undesirable.


### Self-consistency

The ordering should be self-consistent, that is, for all possible template
arguments `T`, `U`, and any unary template `some_template`:

`TYPE-ORDER(T, U) == TYPE-ORDER(some_template<T>, some_template<U>)`.


### Reflection compatibility

Any `operator<=>(std::meta::info, std::meta::info)` should be consistent with this
one.

While `std::meta::info` [@P2320R0] objects can reflect more entities than just
types and values (mainly: expressions), any ordering defined on them should be
finer than this one, and specifically consistent with it. We should do
something that reflection can subsume.

However, it doesn't seem like that proposal will define an ordering on `info`
objects, and we need this solved sooner than that.

### non-goal: Consistency with `type_info::before()`

Ideally, whenever `typeid(x).before(typeid(y)) == true`, then `TYPE-ORDER(x, y) ==
std::strong_ordering::less`. The converse obviously cannot be true, since
`TYPE-ORDER(x, y)` is finer than the order induced by `type_info::before()`.

However, the standard currently says

> The names, encoding rule, and collating sequence for types are all unspecified
> and may differ between programs.

Since this paper requires that the order not differ between programs, exposing
this as a normative requirement is impossible without tightening this
wording, which is outside the scope of this paper.

At least at present, some implementations decide this order at program startup
time, so this is not implementable in general.


## Pros of implementation-defined

The overwhelming response of people working on implementations, as well as
people sitting on the Itanium ABI standards committee, was that this is a
fool's errand, and we need only look at the bugreports for the Itanium ABI to
see why. This view won.


### ABI specifications already have to do this work

As a taster, consider the following conundrum:

```cpp
using what_type = decltype(
        [](this auto&& self,
           decltype([](decltype(self)&){}) x = {}){ return x; }());
        // ^^ outer ^^ inner
```

The inner lambda's "name" is clearly dependent on the outer's, but it also goes
the other way! The ABI standards already have to deal with such mangling
conundrums, and duplicating their work in the C++ standard itself seems highly
counterproductive, especially since the ABI standards already accomplish all
the stability guarantees we would want.


### The C++ standard doesn't own cross-compilation-unit semantics

Another problem is that within the C++ standard, we cannot legislate what
happens with orderings between compilation units. Granted, we do not need this
for `constexpr`, but we do need it to be stable and make sense, so a certain
amount of common sense will be expected from implementations _anyway_.

The recommendation was overwhelmingly to just let the order be
implementation-defined, and let the implementations do the sensible thing.


### Any new proposal to C++ would have to consider the ordering

"punting it off to implementations" saves the C++ standardization process from
having to figure this out for every new proposal that touches types;
implementations, however, already have representation on the committee, and
will veto any truly unimplementable things in this space.


### `TYPE-ORDER(X, Y)` already has public-facing API implications

The "normalized" versions of types will inevitably show up in function
signatures; if different compilers on the same platform produced different
orderings, compilation units from different compilers would refuse to link,
despite both "signatures" beings spelled identically.

Letting the compiler vendors synchronize based on the mangling scheme or
something equivalently useful is the same forum as the current ABI discussion
forums; it seems the appropriate venue to standardize the ordering anyay,
without making WG21 duplicate the work.


## Pros of completely defined order (not chosen)


### Static analyzers do not have a single backend

This proved decisive. Since static analyzers and compilers that produce
intermediate representations before choosing a backend are part of the
ecosystem, it is important to be consistent and not rely on a particular
mangler.


### `consteval` should be as portable as possible

The abstract machine used at constant evaluation time has far fewer parameters
than the runtime one. We should keep them to a minimum, and type ordering is
not a necessary parameter. We should keep implementations consistent.

### Producing and comparing two mangled names takes more memory to cache

The key-tuples proposed in this paper are recursive; the parts of the
type-tuple merely refer to the tuples of the constituent parts; thus, they can
be independently cached and compared as-needed.


# Proposal

## Semantics

Let `X` and `Y` be (possibly cv-ref) qualified types, and `TYPE-ORDER(X, Y)` be an exposition-only macro.

Then, `TYPE-ORDER(X, Y)` denotes a constant expression of type `std::strong_ordering`.

- `std::same_as<X, Y> == true` if and only if `TYPE-ORDER(X, Y) == std::strong_ordering::equal`.
- Otherwise, `TYPE-ORDER(X, Y)` is either `std::strong_ordering::less` or
  `std::strong_ordering::greater`. Which of those is implementation-defined,
  subject to the following semantic constraints.

Implementations must define `TYPE-ORDER(X, Y)` such that it is an ordering, that is,
it is transitive and antisymmetric; that is

- if `TYPE-ORDER(X, Y) == std::strong_ordering::less`, then 
  `TYPE-ORDER(Y, X) == `std::strong_ordering::greater` and vice versa
- if `TYPE-ORDER(X, Y) == std::strong_ordering::less` and `TYPE-ORDER(Y, Z) ==
  std::strong_ordering::less`, then `TYPE-ORDER(X, Z)` is also
  `std::strong_ordering::less`.

Implementations are encouraged, but not required to, make the order recursively-consistent. In other words:

For any class template `template <..., typename Pi, ...> class X`, where `Pi` is the `i`-th template argument,
and any two types `T` and `U`: 
`TYPE-ORDER(T, U) == TYPE-ORDER(X<..., T, ...>, X<..., U, ...>)` if only the `i`th template argument is varied.

If an implementation makes the order recursively consistent, it should document this fact.

## Proposed Syntax

EWG affirmed a library name to access the ordering.

```cpp
// <compare>
template <typename T, typename U>
inline constexpr std::strong_ordering type_order_v = TYPE-ORDER(T, U); /* see below */
template <typename T, typename U>
struct type_order : integral_constant<strong_ordering, type_order_v<T, U>> {};
```

As a special provision for this specific metafunction, we allow the type
arguments for it to be incomplete types.

### Discussion

This seems like a pretty good choice. It does not need a new keyword, only
depends on `<compare>`, and the name seems relatively discoverable.

It's also freestanding, since it doesn't depend on `<typeinfo>`.

We do not want `<type_traits>` to depend on `<compare>`, so we put it into
`<compare>` directly.

### Future extension

Once we have pack aliases, the authors will propose the following two
metafunctions, to be implemented using compiler intrinsics:

```cpp
// as a separate library proposal, once member packs make it
template <typename... Ts>
using ...typemultiset = /* pack of Ts, sorted by type_order_v */;
template <typename... Ts>
using ...typeset = /* uniqued ...typemultiset<Ts...>... */;
```

# Proposed Wording

In [compare.syn]{.sref}, add

::: add

```cpp
template <class T, class U>
struct type_order : integral_constant<strong_ordering, @_see below_@> {};
template <class T, class U>
constexpr strong_ordering type_order_v = type_order<T, U>::value;
```

:::

At the end of [cmp]{.sref}, just before [support.coroutine]{.sref}, add:

:::add

**17.11.7: Type Ordering** [compare.type]

There is an implementation-defined total ordering of all types _TYPE-ORDER_.
The `type_order` class template and `type_order_v` variable template allow querying
the relative positions of pairs of types within this total order.

[1]{.pnum} For types _X_ and _Y_, the expression `@_TYPE-ORDER(X, Y)_@` is a
constant expression [expr.const]{.sref} whose implementation-defined value is
the value of an enumerator of `strong_ordering`, subject to the following
constraints:

- [1.1]{.pnum} `@_TYPE-ORDER(X, Y)_@` is `strong_ordering::equal` if and only if _X_ and _Y_ are the same type
- [1.2]{.pnum} otherwise,
  - [1.2.1]{.pnum} `@_TYPE-ORDER(X, Y)_@` is `strong_ordering::less` if and only if `@_TYPE-ORDER(Y, X)_@` is `strong_ordering::greater` (_antisymmetry_)
  - [1.2.2]{.pnum} for all types _Z_,
    if both `@_TYPE-ORDER(X, Y)_@` and `@_TYPE-ORDER(Y, Z)_@` are `strong_ordering::less`,
    then `@_TYPE-ORDER(X, Z)_@` is also `strong_ordering::less` (_transitivity_)

[Note: `int`, `const int` and `int&` are different types -- end note]

[2]{.pnum} The name `type_order` denotes a _Cpp17BinaryTypeTrait_ (20.15.2) with a base characteristic of `integral_constant<strong_ordering, @_TYPE-ORDER(X, Y)_@>`.

[3]{.pnum} _Recommended practice_: The implementation is encouraged to do the equivalent of alphabetically comparing the linkage-names of the types to allow for consistency across translation units.

[4]{.pnum} _Recommended practice_: The implementation is encouraged to choose an ordering with the following properties:

[4.1]{.pnum} (self-consistency, lexicographical order)
Let _X_ and _Y_ be types, T a class template,
_A_ = _T_<_a_~1~, ..., _a_~n~> and _B_ = _T_<_b_~1~, ..., _b_~n~> specializations of _T_,
and _a_~k~ = _X_ and _b_~k~ = _Y_ for some _k_ be the first differing template
argument between _A_ and _B_ (_a_~i~ = _b_~i~ for all _i_ < _k_).
Then `@_TYPE-ORDER(X, Y)_@ == @_TYPE-ORDER(A, B)_@`.
[Note: the order should be lexicographical on type template arguments, if possible for the implementation -- end note]

[4.2]{.pnum} (argument list consistency)
Let `T` and `U` be class templates, _a~k~_ and _b~k~ be the first differing
template arguments in the type-ids _A_ = _T<a~1~, ..., a_~n~>_ and _B_ = _T<b~1~, ..., b_~n~>_
for some _k_, and let _C_ = _U<a~1~, ..., a_~n~>_ and _D_ = _U<b~1~, ..., b_~n~>_ be valid type-ids.
Then `@_TYPE-ORDER_(_A_, _B_) == @_TYPE-ORDER_(_C_, _D_)_@`.
[Note: template argument lists should impose the same order regardless of the template they apply to -- end note]

:::

Add feature-test macro into [version.syn]{.sref} in section 2

:::add

#define __cpp_lib_type_order 2024XXL // also in <compare>

:::

## Notes on wording

- I'd like to thank Lewis Baker and Davis Herring for reminding me that I should probably make an introductory paragraph.
- Davis Herring suggested adding a note to remind readers that `int`, `const int` and `int&` are different types.
- 

# FAQ

## Why should this be standardized?

Because we have no way to reliably order types across compilation units at compile-time.

## Why not wait for reflection?

It's a good question. However, reflection will do _nothing_ for this problem by
itself; the user will still have to implement ordering using `consteval`
functions, which have no hope of being as fast as a compiler-provided built-in.

User-programmed functions also won't adapt to language evolution; this feature will.

Finally, sorting is arbitrary; having it be consistent throughout the software
ecosystem is potentially a great enabler of interoperability.

## But couldn't this be done faster with reflection?

No; Peter Dimov shares an interesting anecdote.

> I have in Mp11 the algorithm `mp_unique`, which
> takes a list of types and removes the duplicates. In the course of writing
> the reflection papers, their authors occasionally took Mp11 code examples
> and tried to show how they are elegantly implemented using value-based
> reflection metaprogramming.

> So, you take a `vector<info>` that contains types, and then you simply
> apply the existing algorithm `std::unique` to it, et voila... oh wait.

> `std::unique` wants a sorted range, and you can't `std::sort` the info
> vector, because info objects aren't ordered, even when they refer
> to types.


# Implementability

The proposal has no questions on whether it _can_ be implemented - the question
is about the definition of the order.

The concerns raised by the implementers so far have been mostly around having
to bring the name mangler from the backend and make it accessible to the
compiler front-end, because the obvious implementation is to define the
comparison result on the mangled type strings; this option has been rejected by EWG.

Making this order match up with `type_info::before` is a matter of bringing the
name mangler for the correct platform to the frontend.

It is the opinion of the authors that this doesn't seem to be a layering
violation, as the name mangling for a given platform is analogous to other
platform properties, such as the size and alignment of pointers.

If a platform does not have a name mangling strategy, _any_ name mangling
scheme will still result in a standards-conforming implementation; however,
after much discussion, it seems a better direction to completely define the
order in the standard itself.

# Acknowledgements

Thanks to all of the following:

  - Davis Herring for his suggestions on ordering non-type template parameters.
  - Ville Voutilainen for his critique of examples, and providing a simple way
    of explaining the motivation
  - Peter Dimov for a helpful anecdote, now in the FAQ.
  - Erich Keane for for pushing us back to the "implementation-defined" territory.
  - Jens Maurer for his thorough review of the initial proposed wording and his guidance.


# Appendix A: Discarded syntax options

## variable template `std::entity_ordering<X, Y>`

Specifically:

```cpp
template <universal template X, universal template Y>
inline constexpr strong_ordering entity_order_v = TYPE-ORDER(X, Y); /* see below */
template <universal template X, universal template Y>
struct entity_order : integral_constant<strong_ordering, entity_order_v<X, Y>> {};
```

This is a better option than Option 1 if we get universal template parameters,
as we really want to also order class templates, not just types.

However, without universal template parameters, we really don't have much of a
choice but to reach for Option 1.

The name `entity_order` is also slightly less obvious than `type_order`, but
metaprogrammers shouldn't have trouble finding either.


## reflection

Specifically:

```cpp
consteval std::partial_ordering partial_order(std::meta::info x, std::meta::info y) {
    return __comparable(x, y) ? TYPE-ORDER(x, y) : std::partial_order::unordered;
}
```

We could standardize a type order as a function on `std::meta::info` objects in
`std::meta`. However, once we're in `std::meta::info` space, it's more
difficult to know which reflections are comparable and which aren't, so such a
function would need to return a `std::partial_order`, which seems decidedly
less desirable.

It also means we'd need to pass the correct kind of reflection into the
ordering function, which is a bit less intuitive than just `decltype` or just
the template parameter that we already have.


## heterogeneous `constexpr std::type_identity::operator<=>` (bad)

Specifically:

```cpp
template <typename T, typename U>
constexpr std::strong_ordering operator<=>(std::type_identity<T>, std::type_identity<U>);
```

**Pros:**

- No new names.

**Cons:**

- Less discoverable than a new type-trait
- Requires template instantiations of `type_identity<T>`, `type_identity<U>`,
  as well as `operator<=>` overload resolution and substitution, which is quite
  expensive compared to a single `type_order_v<T, U>` direct substitution and
  lookup.
- An extra `operator<=>` overload in the `std` namespace is a drag on overload
  resolution
- adds `<compare>` to `<type_traits>`, since that's where `type_identity` is
- too cute?
- doesn't work for nontypes

## `constexpr std::__lift<arg>::operator<=>` (bad)

This option means we add `template <universal template> struct __lift {};` into
`<type_traits>` and define `operator<=>` for it.

**Pros:**

- ... we'll need a lift sooner or later?

**Cons:**

- all the cons of `type_identity`
- even more nonobvious
- still needs a new name
- needs a tutorial to find


## Non-Option: `constexpr bool std::type_info::before()`

(Included because it's a common question.)

It would be nice, but alas, operates on _cv-unqualified_ versions of the
referenced type, so it's not sufficient.

`constexpr std::strong_order(std::type_info, std::type_info)` has similar
issues.


# Appendix B: building `apply_canonicalized`

We will need a small metaprogramming library; a filter is difficult to do
otherwise.

```cpp
struct undefined;
template <typename... Ts> struct list {};

// apply<F, list<Ts...>> -> F<Ts...>
template <template <typename...> typename, typename> extern undefined _apply;
template <template <typename...> typename F, template <typename...> typename L,
          typename... Ts>
F<Ts...> _apply<F, L<Ts...>>;
template <template <typename...> typename F, typename List>
using apply = decltype(_apply<F, List>);

// concatenate<list<Ts...>, list<Us...>, list<Vs...>> -> list<Ts..., Us..., Vs...>
template <typename...> extern undefined _concatenate;
template <typename... Ts> list<Ts...> _concatenate<list<Ts...>>;
template <typename... Ts, typename... Us, typename... Lists>
decltype(_concatenate<list<Ts..., Us...>, Lists...>)
    _concatenate<list<Ts...>, list<Us...>, Lists...>;
template <typename... Ts>
using concatenate = decltype(_concatenate<Ts...>);

// select: list<T> if true, list<> if false
template <bool v, typename T> extern list<> _select;
template <typename T> list<T> _select<true, T>;

template <bool v, typename T>
using select = decltype(_select<v, T>);
```

Canonicalization is now just a basic not-in-place quicksort-ish thing:

```cpp
template <typename.../*empty*/> extern list<> _canon;
template <typename... Ts>
using canonicalized = decltype(_canon<Ts...>);

// a canonicalized T is just T
template <typename T>
list<T> _canon<T>;

template <typename T, typename... Ts>
concatenate<
    // canonicalized things less than T
    apply<canonicalized, concatenate<select<(TYPE-ORDER(Ts, T) < 0), Ts>...>>,
    list<T> /*T*/, //                        ~~~~~~~~~~~~~~~~
    // canonicalized things greater than T
    apply<canonicalized, concatenate<select<(TYPE-ORDER(Ts, T) > 0), Ts>... >>
    > //                                     ~~~~~~~~~~~~~~~~
_canon<T, Ts...>;
```

We now have `canonicalized<Ts...>` - but this still leaves `list` as a special
type which we'd rather not expose to the user. Onto `apply_canonicalized`:

```cpp
template <template <typename...> typename F, typename... Ts>
using apply_canonicalized = apply<F, canonicalized<Ts...>>;
```

## Full code listing as tested and implemented

Here for completeness, feel free to skip.

```cpp
#include <compare>
#include <type_traits>

struct undefined;

#define TYPE-ORDER(x, y) type_order_v<x, y>

template <typename X, typename Y>
constexpr inline std::strong_ordering type_order_v;

template <template <typename...> typename, typename>
extern undefined _apply;

template <template <typename...> typename F, template <typename...> typename L,
          typename... Ts>
F<Ts...> _apply<F, L<Ts...>>;

template <template <typename...> typename F, typename List>
using apply = decltype(_apply<F, List>);

// some user-type
template <auto x>
struct value_t : std::integral_constant<decltype(x), x> {};
template <auto x>
inline constexpr value_t<x> value_v{};

// built-in
template <auto x, auto y>
constexpr inline std::strong_ordering type_order_v<value_t<x>, value_t<y>> =
    x <=> y;

template <typename... Ts>
struct list {};

template <typename...>
extern undefined _concatenate;
template <typename... Ts>
list<Ts...> _concatenate<list<Ts...>>;
template <typename... Ts, typename... Us, typename... Lists>
decltype(_concatenate<list<Ts..., Us...>, Lists...>)
    _concatenate<list<Ts...>, list<Us...>, Lists...>;

template <typename... Ts>
using concatenate = decltype(_concatenate<Ts...>);

template <bool v, typename T>
extern list<> _select;
template <typename T>
list<T> _select<true, T>;

template <bool v, typename T>
using select = decltype(_select<v, T>);

template <typename...>
extern list<> _canon;
template <typename... Ts>
using canonicalized = decltype(_canon<Ts...>);

template <typename T>
list<T> _canon<T>;

template <typename T, typename... Ts>
concatenate<
    apply<canonicalized, concatenate<select<(TYPE-ORDER(Ts, T) < 0), Ts>...>>,
    list<T>,
    apply<canonicalized, concatenate<select<(TYPE-ORDER(Ts, T) > 0), Ts>... >>
    >
_canon<T, Ts...>;


static_assert(std::same_as<canonicalized<value_t<0>, value_t<-1>, value_t<-1>, value_t<1>>, list<value_t<-1>, value_t<0>, value_t<1>>>);
```

# Appendix C: `__PRETTY_FUNCTION__` instability

This example is available at https://godbolt.org/z/ojb9TnE99 .

Consider the following program, contributed by Barry Revzin:

```cpp
#include <print>

enum class E;
template <E> struct C;
#ifdef DEFINED
enum class E { hi, gašper };
#endif

template <class T>
void show() {
    std::print("{}\n", __PRETTY_FUNCTION__);
}

int main() {
    show<C<E(0)>>();
}
```

When compiled with `-std=c++23`, it yields the following output:

```
void show() [with T = C<(E)0>]
```

However, with `-std=c++23 -DDEFINED`, it produces a different one:

```
void show() [with T = C<E::hi>]
```

This makes external names of types incorporating enums as non-type template
arguments have inconsistent between translation units.

# Appendix D: pros/cons slide from EWG Wroclaw

Implementation-defined or fully specified by the standard?

- Implementation defined:
    - Pro: ABIs already did all the work
    - Cons:
        - ABIs don’t agree
        - Frontend doesn’t know ABI for static analysis tools
        - Layering violation (Erich says that's already happening)
        - Compilers need to agree to have compatible ABI
        - Not necessarily self-consistent (name mangling uses compression) - Erich says it's probably ok
- Fully specified:
    - Pros:
        - Fully portable, including static analysis tools
        - Faster than mangling during constexpr evaluation (Erich says they can short-circuit)
        - (comparison does not require stringifying long symbol names, it short-circuits quickly)
        - Does not require the frontend to know the ABI (helps IDEs) (Erich says not true)
    - Cons:
        - Lots of work
        - Anonymous entities require a completely new-to-standard notion of a “declaration scope” with all the template arguments of all enclosing scopes
        - We need to continue to specify ordering for every change to language entities


# Appendix ZZZ: This his how deep the rabbit hole goes

This section is left in the paper as a hint to the reader of how horrible
specifying all of this would be for the language, without punting it to ABI
specifications.

It is left here as the mess it was at the end of the attempt.

## Foreword

This section sets out an approach for defining an ordering of all compile-time
entities; that is, the definition of `SORT_KEY(X)` for a given _cv_-qualified
type `X`; and the comparison function between these sort-key-tuples.

**Note to reviewers:** _any_ well-defined order will satisfy the design
requirements; the chief design element here is whether we have achieved a
well-ordering, not what exactly it is. The only other considration is whether
we can evolve the order as we introduce new entities into the language.

## Approach

1. We define a **lowering to a _sort-key-tuple_** for every entity in the language.
2. The order is then defined on these _sort-key-tuples_.

The entities we must order are:

1. namespaces and modules
    1. the global namespace
    2. named namespaces
    3. the anonymous namespace
    4. modules
2. cv-qualified types
    1. scalar types
    2. array types
    3. class and union types
    4. class and union template specializations
    5. anonymous versions of all of the above
    6. function types
3. templates
    1. function templates
    2. class templates
    3. variable templates
    4. concepts
    5. type alias templates
    6. deduction guides
4. partial template specializations
    1. partial function template specializations
    2. partial class template specializations
    3. partial variable template specializations
5. parameters
    1. runtime-parameters
    2. type-parameters
    3. constant-parameters
    4. type-template parameters
    5. concept-parameters
    6. variable-template parameters
    7. parameter packs
6. constants
    1. integral constants
    2. floating-point constants
    3. pointer and reference constants
        1. to functions
        2. to named entities of static storage duration
        3. to expressions
    3. class-type constants
        1. lambda literals
        2. constants of compound types usable as template arguments
7. expressions

## Structure of sort-key-tuples

Every _sort-key-tuple_ is of the form `(@_element_@...)`.

where an element is one of:

  - _atom_ (see [atoms](#atoms))
  - _name_
  - _constant_
  - _sort-key-tuple_

These tuples are then ordered lexicographically (ties broken in favor of
shorter tuple), atoms first, then names, then constants, then other _sort-key-tuples_.

Let us name this transformation as `sort_key(entity)`.

The rest of the design is concerned with defining this transformation.

## decl-scopes

A sort-key for most entities is of the form

```
sort_key(x) = (sort_key(decl-scope(x)), x-dependent-elements...)
```

This section deals with the decl-scope of an entity.

To deal with anonymous types and templated entities like hidden friends, we
have to observe more than just the namespace and class the entity is declared
in; we must also observe anything at all that could contribute to the
declaration, such as template parameters and arguments of partial and full
template specializations of both class and function templates,
alias templates, concepts, their parameter numbers (in case of default
arguments), and so on. After that, we must make special allowances for entities
that cannot be influenced by such surroundings, such as forward declaratios of
named classes.

### Every declaration introduces a decl-scope.

- Every declaration _of an anonymous entity_ is ordered lexicographically within
  its decl-scope.
- Ties are not broken; there should be no conflicts, as all the context for a
  given _sort-key(x)_ should be provided by the decl-scope of the declaration.
  (notably - all relevant parameters *and* their values should be part of the
  sort-key for a given declaration of an anonymous entity)
- Named entities' decl-scope is their normal declaration scope.

## What to do with undefined comparisons?

Some comparisons could be left to future revisions because of omissions or
incomplete work. If a program /observes/ such a comparison, the program should
be ill-formed (diagnostic required).

This is to enable fixing the issue in a future revision of the standard. It is
the intention of the authors of this paper that such comparisons should be
exceedingly difficult to observe in practice in normal code, since most
comparisons should be tie-broken fairly early on.

## Modules

Every entity that belongs to a module has that module's full name as a string
as the first part of its `sort_scope`. If an entity belongs to the global
module, it has the atom _global-module_ as its name.

## Namespaces

This section deals with namespaces and their sort-keys.

### The global namespace

Let _x_ be the global namespace.

```
sort_scope(x) := ()
sort_key(x) := (sort_scope(x), _namespace_, _global-namespace_)
```

### Named namespaces

Every namespace (except for the global namespace) has a parent namespace, which
is its immediately enclosing namespace.

let _x_ be some named namespace with the simple-name "namespace_name".

```
sort_scope(x) := sort_key(parent_namespace)
sort_key(x) := (sort_scope(x), _namespace_, "namespace_name")
```

### The anonymous namespace

The anonymous namespace sorts after all the other namespaces.

Let _x_ be some anonymous namespace, with parent namespace `parent_namespace`.

```
sort_scope(x) := sort_key(parent_namespace)
sort_key(x) := (sort_scope(x), _namespace_, _anonymous_)
```

This is ok, as there is only one anonymous namespace per namespace per compilation unit.

### Namespace examples

```
sort_key(::std::ranges) = (
    (
        ((), _namespace_, _global-namespace_), // sort_key(global-namespace)
        _namespace_, "std"
    ), // sort_key(::std)
    _namespace_, "ranges"
);
```

## cv-qualified types

### Qualifiers

Qualifiers are each assigned a score

```
&: 1
&&: 2
const: 3
volatile: 6
```
and ordering lowest-first after summing them.

Any implementation-defined qualifiers get a score that is 2x the largest score
in the table; for instance `__restrict` gets `12`.

Therefore, for an unqualified type `T`, the order of all possible qualified
types would be:

```cpp
0  T
1  T &
2  T &&
3  T const
4  T const &
5  T const &&
6  T volatile
7  T volatile &
8  T volatile &&
9  T const volatile
10 T const volatile &
11 T const volatile &&
```

This is accomplished by putting the qualifier sequence into the tuple just
after the typename.

For a given type `T`, we define

```
qualifier_sequence(T const volatile &) := const volatile &
qualifier_sequence(T const volatile &&) := const volatile &&
```

so that we can put it into the `sort_key` tuple later.

### Scalar Types

All scalar types are built-in types, except for user-defined enumerations,
which should be ordered as if they were class types.

Simple scalar types (everything in this section but function types and pointer
types) are not ordered by "name" - they are ordered using the rules in the
table. All atoms are still ordered before them, any name is ordered after them,
as are all sort-key-tuples.

This is because some of the built-in types do not have names, only type aliases
(such as `decltype(nullptr)`), and we do not order types by their aliases.

This causes any built-in scalar types to be ordered before any compound types.

In case of ties, built-in types with simple names shall be ordered before any
nameless types.

In particular, scalar types shall be ordered as follows:

1. `void` comes first because it's not reifiable,
2. the type of `std::nullptr_t` as the first monostate
3. any other monostates, if added, sorted alphabetically by their common names
   (to be specified explicitly if added)
4. `bool` as the first bi-state
5. any other bi-states, if added, sorted alphabetically.
6. Raw-memory types (`char`, `signed char`, `unsigned char`) (std::byte is an
   enumeration in `std` so it falls under different rules)
7. Integral types in order of size, signed before unsigned (`short`, `unsigned
   short`, `int`, `unsigned int`, `long`, `unsigned long`, `long long`,
   `unsigned long long`, followed by any implementation-defined wider integral
   types like `__int128_t` etc.). Intersperse any implementation-defined
   built-in integral types as needed between the above following those rules.
8. Any remaining character types that are not type-aliases of any of the above,
   including unicode, according to the following rules: smallest first,
   unicode-specific variants after non-unicode variants.
9. Floating-point types, in order of size. In case of ties, `float`, `double`
   and `long double` come before any other floating point types of the same
   size. Any decimal-floating-point types come after binary-floating-point
   types; if multiple floating point types of the same bit-length exist, break
   ties by the bit-size of the exponent, lower first.
10. Implementation-defined vector-register types, ordered by the the integral
    type they consist of, and then by bit-size (example: `u8x16` from gcc's
    documentation).
11. Function types (internally ordered by rules in section [Function Types])
12. Pointer types (internally ordered by their pointee-type)
13. Pointer-to-member types (internally ordered by pointee-type)

### Array Types

TODO: this section is not complete.

Array types shall be ordered after scalar types but before class types.

Given a non-array type `T`

```
sort_key(T[])  := (sort_key(T), [])
sort_key(T[n]) := (sort_key(T), [], n)
```

Multidimensional arrays, as an example:

```
sort_key(int[][n]) = (sort_key(int[]), [], n) = ((sort_key(int), []), [], n);
```

Notice:

```
sort_key(int[]) < sort_key(int[2])
```

because the shorter tuple wins a tie.

### Expressions

Example (courtesy of Lénárd Szolnoki)

```
template <auto x>
struct S {};

template <int x>
void foo(S<2*x>) {}

inline constexpr auto a = &foo<0>;

template <int x>
void foo(S<x>) {}

inline constexpr void (*b)(S<0>) = &foo;

inline constexpr auto x = TYPE-ORDER(S<x>, S<y>); // not equal
```

We must therefore sort arbitrary expressions; I propose we avoid that by making
such comparisons, should they occur, ill-formed, until we are forced to define them.

### Lambdas

A lambda type is an anonymous type, which means it's lexicographically numbered within its `sort_scope`.

Example:

```
template <auto T> C {};
static_assert(type_order_v<
        C<[]{}>, // lambda-key: (sort_key(static_assert), _type_, _anonymous_, 1)
        C<[]{}>  // lambda-key: (sort_key(static_assert), _type_, _anonymous_, 2)
    >
    == std::strong_order::less);
```

TODO: port stuff from bottom.


## Old design that needs to be revamped


### Example 1: `class foo` is declared in `struct bar`: 

```cpp
struct bar { class foo; }
```

```
sort_key(foo) = (sort_key(bar), sort_key(foo)) = ((type, bar), (type, foo, ))
```

This shall hold for any of the above named scopes.

### Example:

Given

```cpp
namespace foo::bar {
    struct i;
}

namespace baz {
  struct j;
}
```

Then:

- `sort_key(foo::bar::i)` is `((namespace, foo), (namespace, bar), (type, i, ))`.
- `sort_key(baz::j)` is `((namespace, baz), (type, j, ))`

When compared, the result is that `baz::j` < `foo::bar::i`, since `namespace
baz` precedes `namespace foo`.

## Atoms

The atoms of _key-tuples_ are ordered as follows:

1. _global-module_
2. _global-namespace_
3. _anonymous_
4. kinds (see [kinds](#kinds))
5. qualifiers (see [qualifiers](#qualifiers))
6. `[]` (array of unknown bound)
7. `*` (pointer)
8. ellipsis (`...` in `f(...)`)
9. parameter pack (`...` in `typename...`)
10. template parameter _auto_
11. template parameter _typename_
12. template parameter T

## identifiers

These are simple names (identifiers).

Examples: "pair", "string", "unique_ptr", "std", "hashmap", "absl".

## Kinds

There are the following kind tokens that can appear in _key-tuples_.

1. value
2. namespace
3. type
4. class template
5. type alias template
6. variable template
7. concept
8. function

Note: everything but "values" is pretty simple, but we haven't dealt with values
extensively yet with the R1 of this paper, though we should just defer to `<=>`
and require a default strong structural ordering for values that may be
template arguments.


### Identifiers

#### Simple Names

Most names are strings that are valid (atomic) identifiers. Those are just themselves:

```cpp
namespace foo::bar { struct baz; }
```

`foo`, `bar` and `baz` are such atomic identifiers.

#### Unnamed entities

Unnamed entities are all assigned a key-tuple of their decl-scope and then 
numbered lexically, consecutively, starting with zero, with the counter being
scoped to their decl-scope.

##### Decl-scopes

Decl-scopes are:

- namespaces
- class definitions
- union definitions
- function declarations
- function default argument definitions
- function definitions
- enumeration definitions
- type aliases
- pack expansions
- template parameter declarations
- definitions of template argument defaults
- class template declarations
- class template definitions
- definitions of partial class template specializations
- definitions of class template specializations
- function template declarations
- function template definitions
- function template default argument definitions
- type alias templates
- concept definitions
- default initializers for class members
 

##### Examples

Function declarations are scoped to the function itself.

Consider a lambda that appears as a default argument of a function template:

```cpp
template <typename T>
void f(T x = []{ return T{0}; }());
//           ^^^^^^^^^^^^^^^^^^ this one
```

The _key-tuple_ for `f<int>` is:

`(function, (f, (type, int)), (type, void), ((type, int)))`

The _key-tuple_ for the lambda is:

`((function, (f, (type, int)), (type, void), ((type, int))), (type, (lambda, 0), ))`.

Note: because of the regular structure of _key-tuples_, such anonymous classes
will compare greater than any entity that has a simple identifier, due to
tuples comparing greater than atoms (which simple names are).

##### Lambda types

As unnamed entities - the types of lambdas are ordered first by where they are
declared, then by declaration (lexical) order.

In effect, we assign them the name `(lambda, #)` where `#` is the count of other
unnamed entities in the decl-scope.

```cpp
namespace Banana {
 auto i = [](int) -> void {}; // 0th lambda instantiated in Banana
}

namespace Apple {
auto i = [](float) -> int {}; // 0th lambda instantiated in Apple
auto j = []() -> std::string {}; // 1st lambda instantiated in Apple
}
```

These would produce the following tuples:

```
sort_key(decltype(Banana::i)) = ((namespace, Banana), (type, (lambda, 0), ));
sort_key(decltype(Apple::i))  = ((namespace, Apple), (type, (lambda, 0), ));
sort_key(decltype(Apple::j))  = ((namespace, Apple), (type, (lambda, 1), ));
```

Note: the empty bit after the identifier is the empty qualifier pack.

##### Unnamed `struct` and `union` types

They are named, respectively, `(class, #)` and `(union, #)`.

**Example:** in a type alias like `typedef struct {} S;`, the unnamed struct type is
decl-scoped to the alias declaration `S`.

### Namespaces

The `sort_key(namespace-name)` is `(namespace, identifier)`.

This means that namespaces are ordered alphabetically by comparing namespace
names at the same rank. A namespace comes before any of its subnamespaces.

Example:

```cpp
namespace outer1 {
  struct i;
}

namespace outer2 {
  namespace inner1 {
    struct i;
  }
  namespace inner2 {
    struct i;
  }
}
```

The order of the three structs w/ type `i` types shall be

`sort_key(outer1::i) < sort_key(outer2::inner1::i) < sort_key(outer2::inner2::i)`.

### Types

The `sort_key` of a type is `(type, <identifier>, <qualifiers>)`.

The `<identifier>` bit is a bit complicated, so let's deal with the qualifiers first.

Note: any decl-scopes the `type` is declared in are part of the parent
_key-tuple_. The `identifier` portion is complicated because of possible
template arguments for types that are template specializations.

### Ordering Class Types

#### Ordering Simple Class Types

Class types shall be greater than scalar types.

Since we cannot redeclare two types with the same name, class types shall be
ordered alphabetically. 

```cpp
struct Apple {};
class Banana {};
struct Carrot {};
```

Would be ordered as `Apple < Banana < Carrot`

As such, we define sort key as:

`sort_key(Apple) = (type, Apple, )`

`sort_key(Banana) = (type, Banana, )`

`sort_key(Carrot) = (type, Carrot, )`

#### Non Type Template Parameters

NTTPs shall first be ordered by their type, then their value.

Given:
```cpp
template <auto T>
struct s {
    decltype(T) i = T;
};

s<1u> a;
s<1.0f> b;
```

`sort_key(s<1u>) = ((type, (s, sort_key(1u))))`

We can define sort_key of `1u` as:
`sort_key(1u) = ( sort_key(decltype(1u)), 1)`

`s<1u>` shall be ordered before `s<1.0f>`, as integral types come before
floating point types.

NTTPs of the same type shall be lexicographically ordered by their scalar
subobjects. Meaning 

```cpp
struct F final {
  struct G final {
    int h;
    int i;
  } g;
  int j;
};

F f{{0,1}, 2};
F f2{{1,2}, 3};
```

`sort_key(s<f>) < sort_key(s<f2>)`;

NTTPs of the same pointer or reference type shall be ordered by instantiation 
order.

#### Ordering Class Template Specializations

Class templates shall be ordered by:

1) Class name, alphabetically.
2) Template arguments, applied lexicographically.

For example, given:
```cpp
template <typename T, typename U>
struct Apple;

struct Banana;
struct Carrot;

Apple<Banana, Carrot>;
Apple<Banana, Banana>;
Apple<Carrot, Carrot>;
```

Note, `sort_key(<parameter>)...` will be used to denote a tuple where `sort_key`
has been applied to all parameters.

For `void f(Foo, Bar)` `sort_key(<parameter>)...` would mean `(sort_key(Foo), sort_key(Bar))`

`sort_key` of a class template shall be defined as:

`sort_key(<class template>) = (type, (<name>, (sort_key(<parameter>)...)))`

So

`sort_key(Apple<Banana, Carrot> = (type, (Apple, (sort_key(Banana), sort_key(Carrot)), )`

`sort_key(Apple<Banana, Carrot> = (type, (Apple, ((type, Banana, ), (type, Carrot, )), )`

Note: the empty bit after the identifier is the empty qualifier pack.

The above would be ordered `sort_key(Apple<Banana, Banana>)`,
`sort_key(Apple<Banana, Carrot>)`, `sort_key(Apple<Carrot, Carrot>`.

#### Function Types

Function types shall be ordered by 

1. Parameters, lexicographically.
2. Return type, if known from the declaration.

The `sort_key` of a function shall be defined as:

`sort_key(<function>) = (function, <name>, (sort_key(<parameter>)...), sort_key(<return type>))`

```cpp
void foo(int i);
```

This function can be represented by:
`(function, (foo, (type, void), ((type, int))))`

```cpp
void foo(int)
void foo(int, double)
```

`sort_key(void foo(int)) = (function, foo, (type, void), ((type, int)))`

`sort_key(void foo(int, double)) = (function, foo, (type, void), ((type, int), (type, double)))`

So, the type of `void foo(int)` would precede the type of `void foo(int, double)`

#### Member Function Types

Function types shall be ordered by 

1. Return type
2. The type of the class it is a member of.
3. Parameters, lexicographically.

The sort key of a member function shall be defined as:

`sort_key(<member function>) =`

`(function, (<name>, sort_key(<class>)), sort_key(<return type>), (sort_key(<parameter>)...))))`

```cpp
struct Foo {
  void bar(int i, float j);
};
```

`sort_key(Foo::bar) = `

`(type, Foo, ), (function, (bar, (type, Foo, )), (type, void), ((type, int, ), (type, float, ))))`

#### Variadic Function Types

Variadic function shall be ordered in a similar way. In a variadic function, the
last argument is a variadic argument. A variadic argument shall be ordered 
immediately after its underlying type.

Given:

```cpp
void foo(Foo);
void foo(Foo...);

```
In this case, the type of `void foo(Foo...)` is ordered immediately after
the type of `void foo(Foo)`.

We can represent these as:

`(function (type, void) (type, Foo, ))`

`(function (type, void) (type, Foo, ...))`

#### Parameter Packs

Parameter are ordered as class templates.

Given:

```cpp
template<class... Types>
struct Tuple {};

class Foo {};
class Bar {};

Tuple<> t0;
Tuple<int> t1;
Tuple<Foo> t2;
Tuple<Bar> t3;
Tuple<Foo, Bar> t4;
```

would be ordered:
`Tuple<>` < `Tuple<int>` < `Tuple<Bar>` < `Tuple<Foo>` < `Tuple<Foo, Bar>`

#### Ordering Class Templates

Kinds of templates are ordered first by name, then by template arguments.

Given:

```cpp
template <template <template<typename> class> class Template>
struct two{};

template <template <typename> class> struct one{};

template <typename> struct zero{};

zero<int> value0;
one<zero> value1;
two<one> value2;
```

These are represented by tuples:

`sort_key(zero<int>) = (type, (zero, (type, int)))`

`sort_key(one<zero>) = (type, (one, (class_template, zero))))`

`sort_key(two<one>) = (type, (two, (class_template, one))))`

#### Variable Templates

Variable templates are ordered by name, then by template parameter.

`sort_key(<variable_template>) = (variable_template, (<name>, (sort_key(<template_parameter>)...)))`

```cpp
template <typename F, typename S>
constexpr std::pair<F, S> pair_one_two = {1, 2};
```

the type of `pair_one_two<int, double>` can be represented as:

`sort_key(pair_one_two<int, double>) = (variable_template, (pair_one_two, (type, int), (type, double)))`

#### Alias Templates

Alias templates are ordered alphabetically by name.

`sort_key(<alias_template>) = (alias_template, <name>)`

Given 
```cpp
template< class T >
using remove_cvref_t = typename remove_cvref<T>::type;
```

`sort_key(remove_cvref_t) = (alias_template, remove_cvref_t)`

#### Concepts

Concepts are ordered in a similar manner to variable templates. 

`sort_key(<concept>) = (concept, (<name>, (sort_key(<template_parameter>)...)))`

```cpp
template <typename T, typename F = decltype([](T){})> 
concept f = requires (T i, F f = [](T){}) {
    {f(i)} -> std::convertible_to<void>;
};
```

In order to order the type of the lambda declared in `concept f`, `concept f` 
must be comparable with other types.

Concepts shall be ordered first by name, then by template arguments.

`sort_key(f<int>) = (concept, (f, (type, int), (lambda, 0)))`

---
references:
  - id: P1907R1
    citation-label: 1
    title: "Inconsistencies with non-type template parameters"
    author:
      family: Maurer
      given: Jens
    issued:
      year: 2019
    URL: https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1907r1.html
  - id: Functional Extensions Library
    citation-label: fn
    title: "Functional Extensions Library"
    author:
        family: Kozicki
        given: Bronek
    issued:
        year: 2024
    URL: https://github.com/libfn/functional
---
