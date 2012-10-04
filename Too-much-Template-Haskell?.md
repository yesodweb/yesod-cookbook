One of the criticisms that Yesod has received for a while now is that it uses too much Template Haskell (TH). Often times, that is the full extent of the criticism, without really explaining why TH is so bad. It seems like the attacks boil down to a few different reasons:

* Since you program in these DSLs so much, it's not longer real Haskell.
* TH code is very difficult to understand.
* You have to learn lots of different syntaxes.

One problem with these arguments is that they completely ignore the fact that there are real reasons to have used TH in the first place. You can say "there's too much TH," but in order to get rid of it, you'll need to either (1) get rid of certain features, or (2) have users write boilerplate code.

The prime example of this is type-safe URLs. To my knowledge, code generation is the only means of achieving the automatic synchronization of datatypes, parsing, and rendering necessary for type-safe URLs. It's true that the special routing syntax is not necessary; we could instead allow users to encode their routes via normal Haskell syntax. But the fact is that *we allow this already*. It's simply a clumsier approach that I've not yet seen any user actually use. In other words, the argument "this new syntax makes it difficult to use" seems to contradict reality, and isn't a true dichotomy anyway.

Returning to the three claims listed above. In reality, there's not really a huge amount of TH code involved in Yesod. We have three different usages of TH, which non-coincidentally correspond to each of the Model, View, and Controller aspects of Yesod:

* Persistent table definitions (Model)
* Shakespearean templates (View)
* Routing/type safe URLs (Controller)

Let's attack the view aspect first. Very few (are there any?) frameworks out there *don't* have a separate syntax for HTML, CSS, and Javascript. These are already different languages, so a new syntax isn't surprising. The fact that we use TH specifically allows us two features that could not be achieved otherwise. All templates are parsed and checked for syntax correctness at compile time, not run time. Additionally, all variable interpolation is checked for type correctness at compile time. Another nice feature is that the templates are compiled directly into the executable, which means they need not be present at runtime. This simplifies deployment and increases performance.

I've already addressed the routing. Persistent is where the strongest argument can be made that TH isn't necessary. And in reality, TH *isn't* necessary: the `persistent` package itself doesn't use a single line of TH. You can write entire Persistent applications without any TH. All you need to do is write the `PersistEntity` instances manually. But this is a tedious, boring exercise. I know of one group of people who do this because the compiler they're using doesn't support TH, but otherwise everyone seems happy to use the TH.

So coming back to the three points listed above, let's try and give them some answers:

* __Since you program in these DSLs so much, it's not longer real Haskell.__ The DSL usage is limited to specific cases. The majority of your code really is normal Haskell code. (I'll grant that typical "hello world" examples give a different impression on this.) But even inside these DSLs, there's still a fair amount of Haskell (e.g., variable interpolation in Shakespeare).
* __TH code is very difficult to understand.__ It might be difficult to *create* TH code, but *using* it is simple. In fact, I will argue that in every case where we use it, the TH code is simpler to write and understand than the corresponding non-TH code.
* __You have to learn lots of different syntaxes.__ You have to learn a routing syntax. I think most people learn that in under five minutes. Hamlet is mostly a simplification of HTML, and our most common comment on it is "I wish HTML worked like this instead." Lucius and Julius are essentially raw CSS and Javascript. The Persistent entities have an incredibly simple syntax as well, I'd be surprised if any user was ever stumped by them.

* * *

And since apparently now the cool thing to talk about is specialized syntaxes instead of "acceptable" TH:

I'm frankly getting very bored with constantly hearing the same complaints against Yesod and its use of specialized syntaxes. I'm going to just put together the most important points here, once, and be done with it. Anyone should feel free to just link to this post.

The fact is: you're not required to use any QQ at all for Yesod. On the extreme side, you could completely ditch TH as well (http://www.haskell.org/pipermail/web-devel/2011/001220.html). Note that that specific project never got off the ground due to lack of interest.

Alternatively, you can keep all the niceness of type-safe URLs and just ditch the QQ syntax for the raw datatypes (https://gist.github.com/3835314).  Clearly we could build combinators on top of those types to make it easier to use, but the point is that everything is available to you now. It's just a choice for the user whether he/she prefers to use a simplified syntax or not.

I get it that many of you don't want to use Yesod. That's OK: we have (at least) two other great frameworks in the Haskell world. Use them if you prefer them. But popping up on every Reddit discussion about Yesod to say that it uses TH and QQ is just a waste of time. All it serves to do is make our community look like one that likes to sit around and bicker instead of just everyone using the tools they each prefer.

And *if* you're going to insist on continuing these comments, please at least try to bring up the caliber a bit. Making general comments about specialized syntaxes being bad is not convincing. Show cases where a specialized syntax in Yesod prevents you from doing something useful. Then we can have an actual discussion about it, and users can make an informed decision whether the trade-off is worth it.

PS: I only addressed the routing syntax in those two links. The exact same solutions work for Persistent. As for Shakespeare: you can easily swap blaze-builder for hamlet, or raw Builder (from text) usage for Lucius and Julius (though a CSS combinator library would be a cool project if anyone is interested).