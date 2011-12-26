-module(plot).
-export([advance/3]).
-include_lib("eunit/include/eunit.hrl").

% advance(State, SpeakingTo, NPCs) -> {NewState, Message, NewNPCs}
advance(got_job, $D, NPCs) ->
  {NewState, Message} = speak(got_job, $D),
  {NewState, Message, kill_dealer(NPCs)};
advance(got_haircut, $G, NPCs) ->
  {NewState, Message} = speak(got_haircut, $G),
  {NewState, Message, revive_dealer(NPCs)};
advance(State, SpeakingTo, NPCs) ->
  {NewState, Message} = speak(State, SpeakingTo),
  {NewState, Message, NPCs}.

kill_dealer(NPCs) ->
  lists:delete({10, 10, $D}, NPCs).
revive_dealer(NPCs) ->
  NPCs ++ [{3,3,$D}].

% speak(State, SpeakingTo) -> {NewState, Message}
speak(start, $D) -> {got_cash, "You're out? Aight. Here's yo lettuice."};
speak(start, $M) -> {start, "You worthless piece of shit. Get out of here."};
speak(start, $G) -> {start, "That Tyrone is lookin' pretty fly wit his new do."};
speak(start, $B) -> {two_jobs, "You can start monday."};
speak(start, $H) -> {pissed_hairdresser, "This aint no charity bitch."};

speak(two_jobs, $D) -> {lost, "I don't think you're taking this seriously *bang*"};
speak(two_jobs, $M) -> {two_jobs, "Glad to hear that."};
speak(two_jobs, $G) -> {two_jobs, "Thats awesome sweety!"};
speak(two_jobs, $B) -> {two_jobs, "I'll see you monday."};
speak(two_jobs, $H) -> {two_jobs, "This aint no charity bitch."};

speak(pissed_hairdresser, $D) -> {lost, "I hear you been hasslin' zanthia *bang*"};
speak(pissed_hairdresser, $M) -> {start, "You worthless piece of shit. Get out of here."};
speak(pissed_hairdresser, $G) -> {start, "That Tyrone is lookin' pretty fly wit his new do."};
speak(pissed_hairdresser, $B) -> {two_jobs, "You can start monday."};
speak(pissed_hairdresser, $H) -> {lost, "Eat scissorts! *stab stab*"};

speak(got_cash, $D) -> {start, "You want back in? Pay up then boy."};
speak(got_cash, $M) -> {got_cash, "You worthless piece of shit. Get out of here."};
speak(got_cash, $G) -> {got_cash, "That Tyrone is lookin' pretty fly wit his new do."};
speak(got_cash, $B) -> {got_job, "I'll see you monday."};
speak(got_cash, $H) -> {early_cut, "Hope you like it."};

speak(early_cut, $D) -> {early_cut, "What? You trying to look like me or something?"};
speak(early_cut, $M) -> {early_cut, "You look stupid."};
speak(early_cut, $G) -> {early_cut, "Babe you looking fine! *kissed 'n' stuff*"};
speak(early_cut, $B) -> {unemployed, "I don't want no hipster bitch."};
speak(early_cut, $H) -> {early_cut, "Hope you like it."};

speak(unemployed, $D) -> {unemployed,"What? You trying to look like me or something?"};
speak(unemployed, $M) -> {lost, "The world's better off without you. Get out."};
speak(unemployed, $G) -> {lost, "I heard. Tyrone's my boy now."};
speak(unemployed, $B) -> {unemployed, "I don't want no hipster bitch."};
speak(unemployed, $H) -> {unemployed, "That cut'll get you ahead in life. Gaurenteed."};

speak(got_job, $D) -> {killed_dealer, "What the fuck yo- Shi- *cough* *gasp*"};
speak(got_job, $M) -> {got_job, "I'm so proud of you son."};
speak(got_job, $G) -> {got_job, "Buy me shit."};
speak(got_job, $B) -> {got_job, "See you monday."};
speak(got_job, $H) -> {early_cut, "I think it suites you."};

speak(killed_dealer, $D) -> error;
speak(killed_dealer, $M) -> {killed_dealer, "I'm so proud of you son."};
speak(killed_dealer, $G) -> {killed_dealer, "I can't believe he's gone."};
speak(killed_dealer, $B) -> {killed_dealer, "Crime 'round here is outa control."};
speak(killed_dealer, $H) -> {got_haircut, "Hope you like it."};

speak(got_haircut, $D) -> error;
speak(got_haircut, $M) -> {won, "You're really on the right track."};
speak(got_haircut, $G) -> {got_laid, "Babe you looking fine! *kissed 'n' stuff*"};
speak(got_haircut, $B) -> {got_haircut, "I miss my son so much."};
speak(got_haircut, $H) -> {got_haircut, "I think it suites you."};

speak(got_laid, $D) -> {won, "Was worth dying to see that shit. *high fives*"};
speak(got_laid, $M) -> {won, "You're really on the right track."};
speak(got_laid, $G) -> {got_laid, "ZZZzzz"};
speak(got_laid, $B) -> {got_laid, "See you monday."};
speak(got_laid, $H) -> {got_laid, "Eeew I heard you guys going at it. Gross."}.

%% Tests

plot_advancement_test() ->
  ExpectedOutput = {won, "You're really on the right track", dealerfull_world()},
  Output = advance(got_laid, $M, dealerfull_world()),
  ?assertEqual(ExpectedOutput, Output).

revive_dealer_test() ->
  Message = "Babe you looking fine! *kissed 'n' stuff*",
  ExpectedOutput = {got_laid, Message, dealer_in_bedroom()},
  Output = advance(got_haircut, $G, dealerless_world()),
  ?assertEqual(ExpectedOutput, Output).

kill_dealer_test() ->
  Message = "What the fuck yo- Shi- *cough* *gasp*",
  ExpectedOutput = {killed_dealer, Message, dealerless_world()},
  Output = advance(got_job, $D, dealerfull_world()),
  ?assertEqual(ExpectedOutput, Output).

dealerless_world() -> [{1,1,$P}].
dealerfull_world() -> [{1,1,$P}, {10,10,$D}].
dealer_in_bedroom() -> [{1,1,$P}, {3,3,$D}].

