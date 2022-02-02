# Statistically-Optimal-Wordle

[Wordle](https://www.powerlanguage.co.uk/wordle/) is all the rage these days. If you're reading this, you probably already know the rules of the game. You get six guesses at a hidden five-letter word randomly selected each day. Each guess must itself be a legitimate five-letter word, and as feedback, you learn whether each letter in your guess is 1. in the target word and in the correct location (`green`), 2. in the target word but not in the correct location (`yellow`), or 3. not in the target word at all (`black`). 

This is my Wordle game from yesterday:

<img src= "figures/fig1.png"/>

Around the Internet, strategy articles abound with titles like ["THE 20 BEST WORDLE STARTING WORDS ACCORDING TO SCIENCE"](https://www.inverse.com/gaming/wordle-starting-words-best-using-math), ["How to crack Wordle: 5-letter words to use first"](https://www.polygon.com/gaming/22884031/wordle-game-tips-best-first-guess-5-letter-words), and ["Best Wordle start words to help you win at Wordle"](https://www.tomsguide.com/news/best-wordle-start-words-to-help-you-win-every-time). 

Pretty much all of these articles are thinking about letter frequency; the best starting word is the one that has all the highest frequency letters in the dictionary. This is a pretty tempting way to go. In fact, if Wordle only told you whether or not each letter was in the target word, it might be pretty close to optimal. The thing is, Wordle also tells you a lot about placement. If a letter is green, you know it's in that place. If it's yellow, you know it's not in that place. This is really valuable information and it would be a shame to throw it out. Unfortunately, it also makes the whole thing a lot more complicated.

As long as we're allowing computers to help us with this guessing game, let's try to get straight to the probabilities involved instead of stopping at frequencies and saying "good enough".

I went on a lot of long roadtrips as a kid, and the most popular game in our family car was "20 Questions". One person thinks of a specific thing--a species of animal, a place, a household appliance--and everyone else has to ask yes-or-no questions to try to guess what that person is thinking of. If they can't get it after 20 questions, the thinker wins. Anyone who has spent any time as a guesser in 20 Questions knows that you shouldn't actually start thinking about what the specific thing is until there are only two possible options of what it could be. Before that, your goal is to narrow down the possibilities as much as possible.

Since Wordle doesn't allow you to guess categories--only individual five-letter words--the narrowing-down game is much trickier than in 20 Questions. On the other hand, Wordle gives you a whole lot more feedback than a yes-or-no answer. Wheras in 20 Questions the optimal question narrows the possibilities by a half, in Wordle we should be able to do much better than that.

How much better? It's time to look at some probability distributions.
