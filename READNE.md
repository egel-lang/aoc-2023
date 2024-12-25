# Egel Solutions to Advent of Code 2023

Marco Devillers

## Day 1 

```
    # Advent of Code (AoC) - day 1, task 2

    import "prelude.eg"
    import "os.ego"

    using System
    using OS
    using List
    using String

    def input =
        let L = read_line stdin in if eof stdin then {} else {L | input}

    def to_digit =
        [ '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 
        | '5' -> 5 | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
        | _ -> none ]

    val table =
        {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"} 
        |> map unpack |> [ XX -> zip XX (from_to 0 9 |> map [N -> chr (48 + N)]) ]

    def prefix = [ XX YY -> XX == take (length XX) YY ]

    def subs =
        [ SUBS {} -> {} | {} {C|CC} -> {C|subs table CC}
        | {(S, D)|SUBS} {C|CC} -> if prefix S {C|CC} then {D,C|subs table CC} else subs SUBS {C|CC} ]

    def main =
        input |> map (do unpack |> subs table |> map to_digit |> filter ((/=) none)) 
              |> map [ XX -> (10 * (head XX)) + (last XX) ] |> sum
```

Advent of Code [day 1](https://adventofcode.com/2024/day/1).

## Day 2 

```
    # Advent of Code (AoC) - day 2, task 2

    import "prelude.eg"
    import "os.ego"
    import "regex.ego"

    using System
    using OS
    using List

    data red, green, blue

    def input =
        let L = read_line stdin in if eof stdin then {} else {L | input}

    val words = Regex::compile "[0-9]+|red|green|blue"

    def to_words =
        do Regex::matches words |> map ["red" -> red | "green" -> green | "blue" -> blue | N -> to_int N ]

    def parse_line =
        do unpack |> break ((==) ':') |> [(L,R) -> (drop 5 L |> pack |> to_words |> head, 
                  split_on ';' (tail R) |> map (do split_on "," |> map (do pack |> to_words |> chunks 2 |> map [{A,B} -> A B]) |> flatten) ) ]

    def to_rgb =
        [ (R red) (_,G,B) -> (R,G,B) | (G green) (R,_,B) -> (R,G,B) | (B blue) (R,G,_) -> (R,G,B) ]

    def max3 = [(R0,G0,B0) (R1,G1,B1) -> (max R0 R1, max G0 G1, max B0 B1)]

    def main =
        input |> map (do parse_line |> [(N, VV) -> (N, map (foldr to_rgb (0,0,0)) VV)])
              |> map [(N, VV) -> foldr max3 (0,0,0) VV] |> map [(R,G,B) -> R*G*B] |> sum
```

Advent of Code [day 2](https://adventofcode.com/2024/day/2).

## Day 3 

```
    # Advent of Code (AoC) - day 3, task 2

    import "prelude.eg"
    import "os.ego"
    import "generator.eg"
    import "dictionary.eg"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def to_board =
       do Gen::from_lists |> Gen::zip_2d Gen::space |> Gen::to_lists |> flatten |> Dict::from_list

    def is_digit = [ C -> let O = String::ord C in and (48 <= O) (O <= 57) ]

    def split_board =
        do Dict::to_list |> filter [ (_, C) -> C /= '.' ] 
            |> split [ (_, C) -> is_digit C ] 
            |> [(D0,D1) -> (Dict::from_list D0, Dict::from_list (filter [(_,C) -> C == '*'] D1))]

    def collate =
        do Dict::to_list |> map [(P,C) -> (P,{C})] 
           |> fix [F {} -> {} | F {X} -> {X} | 
                   F {((X0,Y0), CC0), ((X1, Y1), CC1) | XX} ->
                    if and (X0 == X1) (Y0 == Y1 + 1) then F {((X0, Y1), CC1++CC0)|XX} 
                    else {((X0,Y0), CC0)|F {((X1,Y1), CC1)|XX}} ]

    def neighbours =
        [((X,Y),CC) -> let N = length CC in
            {(X,Y - 1), (X,Y + N)}  ++  
            (map [Y  -> {(X - 1,Y),(X + 1,Y)}] (from_to (Y - 1) (Y + N))) |> flatten ]

    def main =
        input |> map unpack |> to_board |> split_board |> [(D0,D1) -> (collate D0, Dict::keys D1)]
        |> [(NN,SS) -> map [S -> filter [N -> elem S (neighbours N)] NN ] SS]
        |> filter [N -> length N == 2] |> map (map (do snd |> pack |> to_int)) |> map product |> sum
```

Advent of Code [day 3](https://adventofcode.com/2024/day/3).

## Day 4 

```
    # Advent of Code (AoC) - day 4, task 2

    import "prelude.eg"
    import "os.ego"
    import "generator.eg"
    import "dictionary.eg"
    import "regex.ego"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    val number = Regex::compile "[0-9]+|\\|"

    def to_card = do Regex::matches number |> tail |> map to_int |> split_on 0
                     |> [{XX,YY} -> (1, (XX,YY))]

    def count = [(N, (XX,YY)) -> (N, length (filter [Y -> elem Y XX] YY))]

    def process =
        [ {} -> {}
        | {C|CC} -> let (N,M) = count C in let (CC0, CC1) = split_at M CC in
                    {N| process (map [(N0, CC) -> (N0+N, CC)] CC0 ++ CC1)} ]

    def main =
        input |> map to_card |> process |> sum
```

Advent of Code [day 4](https://adventofcode.com/2024/day/4).

## Day 5 

```
    # Advent of Code (AoC) - day 5, task 2

    import "prelude.eg"
    import "os.ego"
    import "regex.ego"

    using System
    using OS
    using List

    def concat_map = [ F XX -> map F XX |> flatten ]

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    val number = Regex::compile "[0-9]+"

    def parse = do split_at 2 |> [(SS, MM) -> 
        (head SS |> Regex::matches number |> map to_int |> chunks 2 |> map [{X,Y} -> (X,X+Y)],
         split_on "" MM |> map (do tail |> map (do Regex::matches number |> map to_int |> [{D,S,N} -> (D,S,N)])))]

    def intersect =
        [(X0, Y0) (X1,Y1) -> 
            if and (X0 <= X1) (X1 < Y0) then (X1, min Y0 Y1)
            else if and (X1 <= X0) (X0 < Y1) then (X0, min Y0 Y1)
            else none ]

    def substract =
        [(X0, Y0) (X1, Y1) ->
            if X0 < X1 then
                if Y0 < X1 then {(X0,Y0)}
                else if Y1 < Y0 then {(X0,X1),(Y1,Y0)}
                else {(X0,X1)}
            else if X1 < X0 then
                if Y1 < X0 then {(X0,Y0)}
                else if Y1 < Y0 then {(Y1,Y0)}
                else {} 
            else if Y1 < Y0 then {(Y1,Y0)}
                else {}]

    def rewrite_seed =
        [{} I -> {I}
        |{(D,S,N)|RR} I -> [none  -> rewrite_seed RR I 
                           |(X,Y) -> {(X-S+D,Y-S+D)|concat_map (rewrite_seed RR) (substract I (X,Y))}] 
                                (intersect I (S,S+N))]

    def rewrites =
        [RRR II -> foldl [II RR -> concat_map (rewrite_seed RR) II] II RRR]

    def main =
        input |> parse |> [(II, RRR) -> rewrites RRR II] |> map fst |> minimum
```

Advent of Code [day 5](https://adventofcode.com/2024/day/5).

## Day 6 

```
    # Advent of Code (AoC) - day 6, task 2 - brute force solution

    import "prelude.eg"
    import "os.ego"
    import "generator.eg"
    import "dictionary.eg"
    import "regex.ego"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    val number = Regex::compile "[0-9]+"

    def parse = do map (do Regex::matches number |> foldr [X Y -> X+Y] "" |> to_int)

    def solutions =
        [{X,T} -> from_to 0 X |> map [T0 -> (X - T0) * T0] |> filter [T0 -> T < T0]] 

    def main =
        input |> parse |> solutions |> length
```

Advent of Code [day 6](https://adventofcode.com/2024/day/6).

## Day 7 

```
    # Advent of Code (AoC) - day 7, task 2

    import "prelude.eg"
    import "os.ego"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def card_to_ord = [C -> index_of C (unpack "J23456789TQKA") ]

    def to_type =
        do sort |> group |> map length |> sort |>
           [{5} -> 6 |{1,4} -> 5 |{2,3} -> 4 |{1,1,3} -> 3|{1,2,2} -> 2 |{1,1,1,2} -> 1 |_ -> 0]

    val cards = unpack "23456789TQKA" |> map card_to_ord

    def jokers = do map [_ -> cards] |> combine

    def best =
        do split ((==) 0) |> [(JJ,CC) -> map ((++) CC) (jokers JJ)] |> map to_type |> maximum

    def main =
        input |> map (do unpack |> split_on ' ' |> [{HH,NN} -> (map card_to_ord HH, to_int (pack NN))])
              |> map [(CC,N) -> (best CC, CC, N)] |> sort
              |> map [(_,_,N) -> N] |> [NN -> zip NN (from_to 1 (length NN))]
              |> map (uncurry (*)) |> sum
```

Advent of Code [day 7](https://adventofcode.com/2024/day/7).

## Day 8 

```
    # Advent of Code (AoC) - day 8, task 2

    import "prelude.eg"
    import "os.ego"
    import "dictionary.eg"
    import "regex.ego"

    using System
    using OS
    using List

    def gcd = [A 0 -> A | A B -> gcd B (A % B)]

    def lcm = [A B -> (A * B) / (gcd A B)]

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    val node = Regex::compile "[0-9|A-Z]+"

    def parse = do split_on "" |> [{{HH}, NN} -> (unpack HH, map (Regex::matches node) NN) ]

    def lr = [ HH N -> List::nth (N % (List::length HH)) HH ]

    def start = 
        [(HH, NN) ->
            let D = foldr [{A,B,C} D -> Dict::set D A (B,C)] Dict::dict NN in
            map [N -> (0, HH, N, D)] (filter [N -> last (unpack N) == 'A'] (Dict::keys D))]

    def step =
        [(N, HH, A, D) -> (N+1, HH, let C = Dict::get D A in ['L' -> fst C |'R' -> snd C] (lr HH N), D)]

    def trace =
        [(N, HH, A, D) -> if last (unpack A) == 'Z' then N else step (N, HH, A, D) |> trace ]

    def main =
        input |> parse |> start |> map trace |> foldl lcm 1
```

Advent of Code [day 8](https://adventofcode.com/2024/day/8).

## Day 9 

```
    # Advent of Code (AoC) - day 9, task 2

    import "prelude.eg"
    import "os.ego"
    import "regex.ego"

    using System
    using OS
    using List

    def zip_with_tail = [F XX -> zip_with F (tail XX) XX]

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    val number = Regex::compile "-?[0-9]+"

    def parse = map (do Regex::matches number |> map to_int)

    def predict = do trace_until (zip_with_tail (-)) (all ((==) 0)) |> map last |> sum

    def main =
        input |> parse |> map reverse |> map predict |> sum
```

Advent of Code [day 9](https://adventofcode.com/2024/day/9).

## Day 10 

```
    # Advent of Code (AoC) - day 10, task 2

    import "prelude.eg"
    import "os.ego"
    import "generator.eg"
    import "dictionary.eg"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def to_board =
        do Gen::from_lists |> Gen::zip_2d Gen::space |> Gen::to_lists |> flatten |> Dict::from_list

    val pipes =
        ['|' -> {(-1,0),(1,0)} |'-' -> {(0,-1),(0,1)} |'L' -> {(-1,0),(0,1)} |'J' -> {(-1,0),(0,-1)} 
        |'7' -> {(0,-1),(1,0)} |'F' -> {(0,1),(1,0)}  |'.' -> {} |'S' -> {(-1,0),(0,-1),(1,0),(0,1)}]

    def add = [(X0,Y0) (X1,Y1) -> (X0+X1,Y0+Y1)]
    def positions = [D P -> do map (add P) |> filter (Dict::has D) ]
    def connect = [D P -> positions D P (pipes (Dict::get D P))]
    def connected = [D P -> connect D P |> filter [P0 -> elem P (connect D P0)]]

    def start = do Dict::to_list |> filter (do snd |> ((==) 'S')) |> head |> fst
    def forward = [{} PP -> PP | {P|_} PP -> filter ((/=) P) PP]
    def loop = [D S PP P -> 
        if and (S == P) (PP /= {}) then PP
        else foldr [P0 none -> loop D S {P|PP} P0|P0 PP -> {P|PP}] none (connected D P |> forward PP) ]

    def update =
        [(D, L) ->
            let F = foldr [P D -> Dict::set D P true] Dict::dict L in
            foldr [P D -> if Dict::has F P then D else Dict::set D P '.'] D (Dict::keys D)]

    def set =
        [ D -> let (MX, MY) = Dict::keys D |> maximum in
               let LL = map [X -> map [Y -> (X,Y)] (from_to 0 MY)] (from_to 0 MX) in 
                foldl [D L -> foldl [(B,C,D) P -> 
                        ['.' -> if B then (B,C,Dict::set D P 'I') else (B,C,D)
                        |'F' -> (B,'F',D) | 'L' -> (B,'L',D) 
                        |'J' -> if C == 'F' then (not B,C,D) else (B,C,D)
                        |'7' -> if C == 'L' then (not B,C,D) else (B,C,D)
                        |'|' -> (not B, C, D) 
                        |_   -> (B,C,D)] (Dict::get D P) ] (false,'.',D) L |> proj 3] D LL ]

    def main =
        input |> map unpack |> to_board |> [D -> (D, loop D (start D) {} (start D))] |> update
        |> set |> Dict::to_list |> map snd |> filter ((==) 'I') |> length
```

Advent of Code [day 10](https://adventofcode.com/2024/day/10).

## Day 11 

```
    # Advent of Code (AoC) - day 10, task 2

    import "prelude.eg"
    import "os.ego"
    import "generator.eg"
    import "dictionary.eg"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def to_board =
        do Gen::from_lists |> Gen::zip_2d Gen::space |> Gen::to_lists |> flatten |> Dict::from_list

    val pipes =
        ['|' -> {(-1,0),(1,0)} |'-' -> {(0,-1),(0,1)} |'L' -> {(-1,0),(0,1)} |'J' -> {(-1,0),(0,-1)} 
        |'7' -> {(0,-1),(1,0)} |'F' -> {(0,1),(1,0)}  |'.' -> {} |'S' -> {(-1,0),(0,-1),(1,0),(0,1)}]

    def add = [(X0,Y0) (X1,Y1) -> (X0+X1,Y0+Y1)]
    def positions = [D P -> do map (add P) |> filter (Dict::has D) ]
    def connect = [D P -> positions D P (pipes (Dict::get D P))]
    def connected = [D P -> connect D P |> filter [P0 -> elem P (connect D P0)]]

    def start = do Dict::to_list |> filter (do snd |> ((==) 'S')) |> head |> fst
    def forward = [{} PP -> PP | {P|_} PP -> filter ((/=) P) PP]
    def loop = [D S PP P -> 
        if and (S == P) (PP /= {}) then PP
        else foldr [P0 none -> loop D S {P|PP} P0|P0 PP -> {P|PP}] none (connected D P |> forward PP) ]

    def update =
        [(D, L) ->
            let F = foldr [P D -> Dict::set D P true] Dict::dict L in
            foldr [P D -> if Dict::has F P then D else Dict::set D P '.'] D (Dict::keys D)]

    def set =
        [ D -> let (MX, MY) = Dict::keys D |> maximum in
               let LL = map [X -> map [Y -> (X,Y)] (from_to 0 MY)] (from_to 0 MX) in 
                foldl [D L -> foldl [(B,C,D) P -> 
                        ['.' -> if B then (B,C,Dict::set D P 'I') else (B,C,D)
                        |'F' -> (B,'F',D) | 'L' -> (B,'L',D) 
                        |'J' -> if C == 'F' then (not B,C,D) else (B,C,D)
                        |'7' -> if C == 'L' then (not B,C,D) else (B,C,D)
                        |'|' -> (not B, C, D) 
                        |_   -> (B,C,D)] (Dict::get D P) ] (false,'.',D) L |> proj 3] D LL ]

    def main =
        input |> map unpack |> to_board |> [D -> (D, loop D (start D) {} (start D))] |> update
        |> set |> Dict::to_list |> map snd |> filter ((==) 'I') |> length
```

Advent of Code [day 11](https://adventofcode.com/2024/day/11).

## Day 12 

```
    #Advent of Code (AoC) - day 12, task 2

    import "prelude.eg"
    import "os.ego"
    import "regex.ego"
    import "dictionary.eg"

    using System
    using OS
    using List

    def reduce = [F {X} -> X | F {X,Y|XX} -> reduce F {F X Y|XX}]

    def lexeme = Regex::compile "[\\.|\\?|#]+|[0-9]+"

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def parse = do Regex::matches lexeme |> [ XX -> (unpack (head XX), map to_int (tail XX))]

    def expand = [(XX, NN) -> (repeat 5 XX |> reduce [XX YY -> XX ++ {'?'} ++ YY], repeat 5 NN |> flatten)]

    def count = 
        [F ({'.'},NN) -> if NN == {} then 1 else 0
        |F ({'#'},NN) -> if NN == {1} then 1 else 0
        |F ({'?'|XX},NN) -> F ({'.'|XX},NN) + F ({'#'|XX},NN)
        |F ({'.'|XX},NN) -> F (XX,NN)
        |F ({'#','.'|XX},{1|NN}) -> F ({'.'|XX},NN)
        |F ({'#','.'|XX},NN) -> 0
        |F ({'#','#'|XX},{1|NN}) -> 0
        |F ({'#','#'|XX},{N|NN}) -> F ({'#'|XX},{N - 1|NN})
        |F ({'#','?'|XX},NN) -> F ({'#','.'|XX},NN) + F ({'#','#'|XX},NN)
        |F (XX,{}) -> 0 ]

    def memo = [D F X -> if Dict::has D X then Dict::get D X else let Y = F (memo D F) X in Dict::set D X Y; Y]

    def main =
        input |> map parse |> map expand |> map (memo Dict::dict count) |> sum
```

Advent of Code [day 12](https://adventofcode.com/2024/day/12).

## Day 13 

```
    #Advent of Code (AoC) - day 13, task 2

    import "prelude.eg"
    import "os.ego"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def parse = do split_on "" |> map (map unpack)

    def differ = [X Y -> if X /= Y then 1 else 0]

    def reflect = 
        do [XX -> map [N -> (N, split_at N XX)] (from_to 1 (length XX - 1))]
        |> map [(N,(XX,YY)) -> (N, zip (reverse XX) YY)] 
        |> map [(N,XX) -> (N, map [(XX,YY) -> zip XX YY |> map (uncurry differ) |> sum] XX |> sum)]
        |> filter [(N,D) -> D == 1] |> map fst

    def main =
        input |> parse 
        |> [XX -> concat_map reflect (map transpose XX) ++ map ((*) 100) (concat_map reflect XX)] 
        |> sum
```

Advent of Code [day 13](https://adventofcode.com/2024/day/13).

## Day 14 

```
    #Advent of Code (AoC) - day 14, task 2

    import "prelude.eg"
    import "os.ego"
    import "dictionary.eg"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def slide1 =
        [ XX {}  -> XX
        | XX {'#'|YY} -> XX ++ {'#'| slide1 {} YY}
        | XX {'O'|YY} -> {'O' | slide1 XX YY}
        | XX {'.'|YY} -> slide1 {'.'|XX} YY ]

    #def slide = do transpose |> map (slide1 {}) |> transpose
    #def turn = do transpose |> map reverse
    #def cycle = iter 4 (do slide |> turn) 

    def cycle = iter 4 (do transpose |> map (slide1 {}) |> map reverse)

    def count =
        do transpose |> concat_map [ XX -> zip (from_to 1 (length XX)) (reverse XX) ]
        |> filter (do snd |> ((==) 'O')) |> map fst |> sum

    def iter_fast = 
        [D I N F X -> 
            if I == N then X
            else if Dict::has D X then let J = Dict::get D X in
                Dict::get (Dict::inverse D) (J+((N-J)%(I-J))) |> head
            else Dict::set D X I; iter_fast D (I+1) N F (F X)]

    def main =
        input |> map unpack |> iter_fast Dict::dict 0 1000000000 cycle |> count

```

Advent of Code [day 14](https://adventofcode.com/2024/day/14).

## Day 15 

```
    #Advent of Code (AoC) - day 15, task 2

    import "prelude.eg"
    import "os.ego"
    import "dictionary.eg"
    import "regex.ego"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def groups = [ R -> let C = Regex::compile R in do Regex::matches C |> map (Regex::group C) ]

    def parse = do head |> groups "([a-z]+)-|([a-z]+)=([0-9]+)" |> map [{X,"",""} -> unpack X | {_,X,Y} -> (unpack X, to_int Y) ]

    def hash = foldl [N C -> ((N + String::ord C) * 17) % 256] 0

    def boxes = Dict::from_list (from_to 0 255 |> map [N -> (N,{})])

    def boxes_adjust = 
        [D (L,N) -> Dict::adjust D (hash L) (do break (do fst |> ((==) L)) |> [(XX,{}) -> XX ++ {(L,N)} |(XX,YY) -> XX ++ {(L,N)} ++ (tail YY)])
        |D L -> Dict::adjust D (hash L) (filter (do fst |> ((/=) L))) ]

    def count =
        do Dict::to_list |> concat_map [(N, LL) -> zip_with [(L,F) P -> (N+1) * F * P] LL (from_to 1 (length LL))] |> sum

    def main =
        input |> parse |> foldl boxes_adjust boxes |> count
```

Advent of Code [day 15](https://adventofcode.com/2024/day/15).

## Day 16 

```
    # Advent of Code (AoC) - day 16, task 1
    import "prelude.eg"
    import "os.ego"
    import "generator.eg"
    import "dictionary.eg"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def to_board =
       do Gen::from_lists |> Gen::zip_2d Gen::space |> Gen::to_lists |> flatten |> Dict::from_list

    def print_board =
        [ B -> let KK = Dict::keys B in
            foldl [(X0,Y0) (X1,Y1) -> (if not (X0 == X1) then print "\n" else none);
                   print (Dict::get B (X1, Y1)); (X1,Y1) ] (head KK) KK; none ]

    def add = [(X0,Y0) (X1,Y1) -> (X0+X1,Y0+Y1)]
    def mirror = [(X,Y) -> (-X,-Y)]
    def step = [(P,V) -> (add P V, V)]
    def horizontal = do fst |> ((==) 0)

    def beam =
        [D (P,V) ->
            [ none -> {}
            | '.'  -> beam D (step (P,V))
            | '|'  -> if horizontal V then {step (P,swap V), step (P,mirror (swap V))} else {step (P,V)}
            | '-'  -> if horizontal V then {step (P,V)} else {step (P,swap V), step (P,mirror (swap V))}
            | '/'  -> {step (P, mirror (swap V))}
            | '\\' -> {step (P, swap V)} ]
            (if Dict::has D P then Dict::get D P else none) ]

    def fix_set = 
        [ F YY {}     -> YY
        | F YY {X|XX} -> if elem X YY then fix_set F YY XX else fix_set F {X|YY} (F X ++ XX) ]

    def energize =
        [D E (P,V) ->
            [ none -> E
            | '.'  -> energize D (Dict::set E P '#') (step (P,V))
            | _    -> Dict::set E P '#' ] (if Dict::has D P then Dict::get D P else none) ]

    def count = [D S -> print S "\n"; fix_set (beam D) {} {S} |> foldl (energize D) Dict::dict |> Dict::keys |> length]  

    def edges =
        do Dict::keys |> maximum |> [(MX,MY) ->
        let UP = {from_to 0 MX,{0},{(0,1)}} in let DOWN = {from_to 0 MX,{MY},{(0,-1)}} in
        let LEFT = {{0},from_to 0 MY,{(1,0)}} in let RIGHT = {{MX},from_to 0 MY,{(-1,0)}} in 
            concat_map combine {UP,DOWN,LEFT,RIGHT} |> map [{X,Y,V} -> ((X,Y),V)] ]

    def main = input |> map unpack |> to_board |> [D -> map (count D) (edges D)] |> maximum
```

Advent of Code [day 16](https://adventofcode.com/2024/day/16).

## Day 17 

Missed this day.

Advent of Code [day 17](https://adventofcode.com/2024/day/17).

## Day 18 

```
    # Advent of Code (AoC) - day 18, task 2

    import "prelude.eg"
    import "os.ego"
    import "dictionary.eg"
    import "regex.ego"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def group = do Regex::compile |> [R -> Regex::group R] 

    def parse = do map (group "([A-Z]) ([0-9]+) \\(#([0-9a-f]+)\\)") 
                        |> map [{D,L,RGB} -> (head (unpack D), to_int L, unpack RGB)]

    def mul = [N (X,Y) -> (N*X,N*Y)]

    def add = [(X0,Y0) (X1,Y1) -> (X0+X1,Y0+Y1)]

    def product = [(X0,Y0) (X1,Y1) -> (X0 * Y1) - (X1 * Y0) ]

    def points =
        let F = ['U' -> (-1,0) |'D' -> (1,0) |'L' -> (0,-1) |'R' -> (0,1)] in
        do foldl [{P|PP} (D,L,_) -> {add (mul L (F D)) P,P|PP}] {(0,0)} |> reverse

    def shoelace =
        [PP -> let N = length PP in 
            map [I -> product (List::nth I PP) (List::nth ((I+1)%N) PP)] (from_to 0 (N - 1))
            |> sum |> [N -> (abs N)/2] ]

    def perim = do map (proj 2) |> sum |> [N -> N/2]

    def hex =
        do map
            ['0' -> 0 |'1' -> 1 |'2' -> 2 |'3' -> 3 |'4' -> 4 |'5' -> 5 |'6' -> 6 |'7' -> 7 
            |'8' -> 8 |'9' -> 9 |'a' -> 10 |'b' -> 11 |'c' -> 12 |'d' -> 13 |'e' -> 14 |'f' -> 15 ]
        |> foldl [N D -> N*16+D] 0

    def adjust = [(_,_,RGB) -> 
        (['0' -> 'R' | '1' -> 'D' | '2' -> 'L' | '3' -> 'U'] (last RGB), hex (init RGB), RGB)]

    def main =
        input |> parse |> map adjust |> [PP -> (perim PP) + (points PP |> shoelace) + 1]
```

Advent of Code [day 18](https://adventofcode.com/2024/day/18).

## Day 19 

```
    import "prelude.eg"
    import "search.eg"
    import "dictionary.eg"

    using OS, System, List, Search

    def input = let L = read_line stdin in if eof stdin then nil else cons L input                                                               

    def look = [{} -> fail {} | {T|TT} -> success T {T|TT} ]
    def skip = [{} -> success none {} | {_|TT} -> success none TT]
    def match = [S0 -> look <*> [(T, S1) -> if S0 == S1 then skip <*> \_ -> success S0 else fail]]
    def tag = [T0 -> look <*> [(T1, S) -> if T0 == T1 then skip <*> \_ -> success S else fail]]
    def is_name = tag "lowercase" <+> tag "uppercase"
    def is_op = tag "operator"
    def is_num = tag "integer" <@> \S -> (to_int S)

    def parse_guard = (is_name <+> is_num) <*> \C0 -> is_op <*> \OP -> (is_name <+> is_num) <@> \C1 -> (C0 OP C1)
    def parse_command = parse_guard <*> \G -> match ":" <*> \_ -> is_name <@> \T -> (G, T)
    def parse_rule = (parse_command <+> is_name) </> \R -> match "," <@> \_ -> R
    def parse_flow = is_name <*> \N -> match "{" <*> \_ -> star parse_rule <*> \RR -> match "}" <@> \_ -> (N,RR)

    def parse_dim = is_name <*> \_ -> match "=" <*> \_ -> is_num </> \N -> match "," <@> \_ -> N
    def parse_part = match "{" <*> \_ -> star parse_dim <*> \DD -> match "}" <@> \_ -> [{X,M,A,S} -> (X,M,A,S)] DD

    def parse_lines = search (star (parse_flow <+> parse_part)) [X CC -> X] [X CC -> throw X] [X CC -> throw X]

    def rules = do foldl (+) "" |> tokenize "" |> map [(P,T,S) -> (T,S)] |> parse_lines |> filter [(P, RR) -> true | _ -> false]

    def xmas = ["x" -> 0 | "m" -> 1 | "a" -> 2 | "s" -> 3]

    def cube_split = [D N C -> (proj_update D [(X,Y) -> (X,N)] C, proj_update D [(X,Y) -> (N+1,Y)] C)]
    def cube_size = do tuple_to_list |> map [(X,Y) -> 1 + Y - X] |> product
    def is_cube = do tuple_to_list |> all [(X,Y) -> X <= Y]
    def space = ((1,4000),(1,4000),(1,4000),(1,4000))

    def next0 =
        [{((T "<" N), B)|RR} C -> 
            let (C0,C1) = cube_split (xmas T) (N - 1) C in
            {(B, C0)|next0 RR C1}
        |{((T ">" N), B)|RR} C -> 
            let (C0,C1) = cube_split (xmas T) (N - 0) C in
            {(B, C1)|next0 RR C0}
        |{B} C -> {(B, C)}]

    def next = [RR C -> next0 RR C |> filter [(_,C) -> is_cube C]]

    def process =
        [D {("R", C)|AA} -> process D AA
        |D {("A", C)|AA} -> {("A", C)| process D AA}
        |D {(B,C)|AA}    -> process D (next (Dict::get D B) C ++ AA)
        |D {}            -> {} ]

    def main = 
        input |> rules |> Dict::from_list |> [D -> process D {("in", space)}]
        |> map snd |> map cube_size |> sum
```

Advent of Code [day 19](https://adventofcode.com/2024/day/19).

## Day 20 

Missed this day.

Advent of Code [day 20](https://adventofcode.com/2024/day/20).

## Day 21 

```
    # Advent of Code (AoC) - day 21, task 2
    import "prelude.eg"
    import "python3.ego"

    using System

    def program = """
    # Program by Daniel Lawrence Lu. https://daniel.lawrence.lu/
    from collections import deque

    import numpy as np


    def zxcv(fname, n):
        with open(fname) as f:
            dat1 = f.read().strip().split("\\n")

        if n > 64:
            data = []
            for i in range(5):
                for line in dat1:
                    data.append(5 * line.replace("S", "."))
        else:
            data = dat1

        width = len(data[0])
        height = len(data)

        q = deque()

        sx, sy = width // 2, height // 2

        q.append((sx, sy, 0))
        s64 = set()
        visited = set()

        while q:
            x, y, steps = q.popleft()
            if (x, y, steps) in visited:
                continue
            visited.add((x, y, steps))
            if steps == n:
                s64.add((x, y))
            else:
                if x >= 0:
                    if data[y][x - 1] != "#":
                        q.append((x - 1, y, steps + 1))
                if x < width - 1:
                    if data[y][x + 1] != "#":
                        q.append((x + 1, y, steps + 1))
                if y >= 0:
                    if data[y - 1][x] != "#":
                        q.append((x, y - 1, steps + 1))
                if y < height - 1:
                    if data[y + 1][x] != "#":
                        q.append((x, y + 1, steps + 1))

        if n == 64:
            for y, line in enumerate(data):
                ll = list(line)
                for sx, sy in s64:
                    if sy == y:
                        ll[sx] = "O"
                print("".join(ll))

        return len(s64)


    zxcv("test0.txt", 2)
    print("part 1:", zxcv("input.txt", 64))

    # polynomial extrapolation
    a0 = zxcv("input.txt", 65)
    a1 = zxcv("input.txt", 65 + 131)
    a2 = zxcv("input.txt", 65 + 2 * 131)

    vandermonde = np.matrix([[0, 0, 1], [1, 1, 1], [4, 2, 1]])
    b = np.array([a0, a1, a2])
    x = np.linalg.solve(vandermonde, b).astype(np.int64)

    # note that 26501365 = 202300 * 131 + 65 where 131 is the dimension of the grid
    n = 202300
    print("part 2:", x[0] * n * n + x[1] * n + x[2])
    """

    val python3 = Python::run none

    def main = Python::eval program
```

Advent of Code [day 21](https://adventofcode.com/2024/day/21).

## Day 22 

Missed this day.

Advent of Code [day 22](https://adventofcode.com/2024/day/22).

## Day 23 

```
    # Advent of Code (AoC) - day 23, task 1

    # only for the example

    import "prelude.eg"
    import "os.ego"
    import "generator.eg"
    import "dictionary.eg"
    import "pqueue.ego"

    using System
    using OS
    using List

    def input = let L = read_line stdin in if eof stdin then {} else {L | input}

    def to_board =
       do Gen::from_lists |> Gen::zip_2d Gen::space |> Gen::to_lists |> flatten |> Dict::from_list

    def print_board =
        [ B -> let KK = Dict::keys B in
            foldl [(X0,Y0) (X1,Y1) -> (if not (X0 == X1) then print "\n" else none);
                   print (Dict::get B (X1, Y1)); (X1,Y1) ] (head KK) KK; none ]

    def parse = do map unpack |> to_board

    def mul = [N (X,Y) -> (N*X,N*Y)]

    def add = [(X0,Y0) (X1,Y1) -> (X0+X1,Y0+Y1)]

    val dirs = {(0,1),(0,-1),(1,0),(-1,0)}

    def start = (0,1)

    def end = do Dict::keys |> maximum

    def next =
        [D P -> Dict::get D P 
        |> ['#' -> {} 
           |_ -> map (add P) dirs]
        |> filter (Dict::has D)
        |> filter (do Dict::get D |> ((/=) '#')) ]

    def to_graph = [D -> Dict::keys D |> filter (do Dict::get D |> ((/=) '#')) |> map [P -> (P, next D P)] ]

    def hike =
        [G P E S ->
            if S == E then {{S}}
            else if elem S P then {}
            else Dict::get G S |> map (hike G {S|P} E) 
                 |> filter ((/=) {}) |> flatten |> map (cons S) ]

    def main =
        input |> parse |> to_graph |> Dict::from_list |> [G -> hike G {} (end G) start]
        |> map length |> map [X -> X - 1] |> maximum
```

Advent of Code [day 23](https://adventofcode.com/2024/day/23).

## Day 24

``` ```

Advent of Code [day 24](https://adventofcode.com/2024/day/24).

## Day 25

``` ```

Advent of Code [day 25](https://adventofcode.com/2024/day/25).

## Line, word, and character count

```
    day  1 -   31  202   909
    day  2 -   32  194  1013
    day  3 -   39  257  1404
    day  4 -   28  130   719
    day  5 -   50  258  1512
    day  6 -   23  104   548
    day  7 -   29  170   884
    day  8 -   36  213   981
    day  9 -   22   95   512
    day 10 -   50  380  2061
    day 11 -   32  194   985
    day 12 -   37  221  1201
    day 13 -   25  142   703
    day 14 -   38  207  1069
    day 15 -   30  194  1009
    day 16 -   54  364  1993
    day 17 -    0    0     0
    day 18 -   46  288  1423
    day 19 -   55  490  2482
    day 20 -    0    0     0
    day 21 -   88  310  2119
    day 22 -    0    0     0
    day 23 -   55  267  1434
    day 24 -    0    0     0
    totals -  800 4680 24961
```

## Lines of code, token, and unique token count

```
    day  1 -   16   262   94
    day  2 -   15   318   85
    day  3 -   22   440   98
    day  4 -   11   184   67
    day  5 -   32   435   82
    day  6 -    7   115   59
    day  7 -   14   263   73
    day  8 -   16   311   82
    day  9 -    7   108   55
    day 10 -   32   707  111
    day 11 -   16   278   81
    day 12 -   19   436   76
    day 13 -   12   199   63
    day 14 -   18   252   77
    day 15 -   12   308   89
    day 16 -   35   650  111
    day 17 -    0     0    0
    day 18 -   24   476  119
    day 19 -   38   793  141
    day 20 -    0     0    0
    day 21 -   59    24   17
    day 22 -    0     0    0
    day 23 -   29   425   96
    day 24 -    0     0    0
    totals -  434  6984

    -- excluding import and using
```

Copyright 2023 Marco Devillers, MIT licence
