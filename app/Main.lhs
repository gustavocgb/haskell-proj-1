Setup inicial
-------------

> {-# OPTIONS_GHC -Wall #-}

> module Main where

> -- Adicionei a lib Data.Char
> import Data.Char (ord)
> import Test.Tasty
> import Test.Tasty.HUnit

> main :: IO ()
> main = defaultMain tests

> tests :: TestTree
> tests
>   = testGroup "Unit tests"
>         [
>            question01Tests
>         ,  question02Tests
>         ,  question03Tests
>         ,  question04Tests
>         ,  question05Tests
>         ]


Questão 1. Escreva a função

> --question01 :: [Integer] -> [Integer]
> --question01 = undefined

que recebe uma lista de inteiros como entrada e
retorna como resultado uma lista de inteiros em que
todo número ímpar presente na lista é elevado ao quadrado.
Sua implementação deve atender os seguintes casos de teste.

> -- Explicacao
> -- Como de acordo com o enunciado, verificar se e impar e se for elevar ao quadrado,
> -- pensei em iterar sobre a cabeça da lista fazendo a verificacao. Como o retorno da funcao e uma lista, usei a funcao map para abstrair a recursao
> -- e criei a funcao question01' que faz a verificao mencionada.

> question01 :: [Integer] -> [Integer]
> question01 [] = []
> question01 xs = map question01' xs
>   where
>     question01' x
>       | odd x = x*x
>       | otherwise = x

> question01Tests :: TestTree
> question01Tests
>       = testGroup "Question 01 Tests"
>                    [
>                       testCase "Question 01 empty" $
>                           question01 [] @?= []
>                    ,  testCase "Question 01 all even" $
>                           question01 (map (* 2) [1..5]) @?= map (* 2) [1..5]
>                    ,  testCase "Question 01 some odd" $
>                           question01 [1..5] @?= [1,2,9,4,25]
>                    ]


Questão 2. Considere o seguinte tipo de dados:

> data Times = Zero | One | Two

Sua tarefa é implementar a função:

> --question02 :: Times -> (a, a, a) -> (a, a, a)
> --question02 = undefined

que a partir de um valor do tipo `Times` e uma tripla
de valores de tipo `a`, retorna uma tripla na qual os
valores foram rotacionados um número de vezes especificado
pelo tipo `Times.` Os casos de teste a seguir apresentam
exemplos desta função.

> -- Explicacao
> -- Como a funcao recebe uma tripla e retorna uma tripla e Times possui tres valores, relativos as rotacoes da tripla, pensei
> -- em somente realizar as combinacoes que estao nos casos de testes.

> question02 :: Times -> (a, a, a) -> (a, a, a)
> question02 Zero (x,y,z) = (x,y,z)
> question02 One (x,y,z) = (z,x,y)
> question02 Two (x,y,z) = (y,z,x)

> question02Tests :: TestTree
> question02Tests
>       = testGroup "Question 02 Tests"
>                   [
>                      testCase "Swapping Zero times:" $
>                           question02 Zero ("a","b","c") @?= ("a","b","c")
>                   ,  testCase "Swapping One time:" $
>                          question02 One ("a", "b", "c") @?= ("c", "a", "b")
>                   ,  testCase "Swapping Two times:" $
>                           question02 Two ("a", "b", "c") @?= ("b", "c", "a")
>                   ]


Questão 03. Considere o seguinte tipo de dados que representa
dados de clientes de uma loja:

> type Name  = String
> type Phone = String
> type Email = String

> data Client = Client Name Phone Email deriving (Eq, Show)

> --question03 :: Client -> Bool
> --question03 = undefined

Dizemos que a informação de um cliente é válida se:

a) O nome do cliente possui pelo menos 3 caracteres e é
formado exclusivamente por letras e espaços.

b) A informação de telefone é composta apenas por dígitos

c) A string de email deve conter o caractere `@` e ter tamanho
maior que 3.

Com base nessas informações, desenvolva a função:
que verifica se a informação de cliente é ou não válida de
acordo com as regras mencionadas anteriormente.

Sua implementação deve considerar os seguintes casos de teste.

> -- Explicacao
> -- Essa questao possui um Client que contem Nome, Phone e Email. Como mencionado no enunciado
> -- cada atributo possui uma restricao para satisfazer a validacao do Client, pensei em criar funcoes
> -- para validarem cada restricao. Para realizar operacoes com os caracteres usei a lib Data.Char
> -- com ela transfomo Char em Int, slide da aula oito, para realizar as operacoes, e retorno Bool
> -- informando o status da valicao.

> question03 :: Client -> Bool
> question03 (Client xs ys zs )
>   | verifyLetter xs && verifyTreeCaractere xs
>       && verifyDigit ys
>       && verifyTreeCaractere zs && verifyEmailCaractere zs = True
>   | otherwise = False

> -- Verifica se no minimo 3 caracteres
> verifyTreeCaractere :: String -> Bool
> verifyTreeCaractere xs
>   | length xs < 3 = False
>   | otherwise = True

> -- Verifica se e digito, de 0 a 9. Na tabela asc 48 a 57
> verifyDigit :: String -> Bool
> verifyDigit [] = True
> verifyDigit (x:xs)
>   | char2Int x >= 48 && char2Int x <= 57 = verifyDigit xs
>   | otherwise = False

> -- Verifica se e letra, de a ate z, e A ate Z. Na tabela asc 65 a 90 e 97 a 122
> verifyLetter :: String -> Bool
> verifyLetter [] = True
> verifyLetter (x:xs)
>   | (char2Int x >= 65 && char2Int x <= 90) || (char2Int x >= 97 && char2Int x <= 122) = verifyLetter xs
>   | otherwise = False

> -- Funcao que transforma um char em int de acordo com a tabela asc
> char2Int :: Char -> Int
> char2Int = ord

> -- Verifica possui o caractere '@'
> verifyEmailCaractere :: String -> Bool
> verifyEmailCaractere [] = False
> verifyEmailCaractere (x:xs)
>   | char2Int x /= 64 = verifyEmailCaractere xs
>   | otherwise = True


> question03Tests :: TestTree
> question03Tests
>       = testGroup "Question 03 Tests"
>                   [
>                      testCase "Valid client" $
>                        question03 (Client "Marcos" "123456789" "marcos@bla.com") @?= True
>                   ,  testCase "Invalid name - size" $
>                        question03 (Client "Mr" "123456789" "marcos@bla.com") @?= False
>                   ,  testCase "Invalid name - not all letters" $
>                        question03 (Client "Mr22" "123456789" "marcos@bla.com") @?= False
>                   ,  testCase "Invalid phone" $
>                        question03 (Client "Marcos" "ab23" "marcos@bla.com") @?= False
>                   ,  testCase "Invalid email - size" $
>                        question03 (Client "Marcos" "123456789" "m@") @?= False
>                   ,  testCase "Invalid email - lacking @" $
>                        question03 (Client "Marcos" "123456789" "marcobla.com") @?= False
>                   ]

Questão 04. Um inconveniente da solução apresentada no exercício 03 é que a função não
apresenta uma explicação do motivo da validação falhar. Uma alternativa para isso é
criar um tipo de dados para representar as possíveis falhas de validação.

> data Error = NameLengthError       -- invalid size
>            | NameCharactersError   -- name with non-letters and space characters
>            | PhoneError            -- phone with non numeric chars.
>            | EmailSizeError        -- invalid size
>            | EmailCharError        -- lacking `@`
>            deriving (Eq, Show)


Usando a representação de erros de validação, podemos definir um tipo para representar
a validação:

> data Validation = Ok
>                 | Failure [Error] deriving (Eq, Show)

O construtor `Ok` representa que a validação executou com sucesso e o construtor `Failure`
representa uma falha de validação e armazena uma lista dos erros encontrados.

Com base no apresentado, implemente a função.

> -- question04 :: Client -> Validation
> -- question04 = undefined

que realiza a validação de clientes, como apresentado na questão 03, e retorna um valor do
tipo `Validation`. Sua implementação deve atender os seguintes casos de teste.

> -- Explicacao
> -- Para representar as validacoes mencionadas em Validation, tanto no caso de erro e de sucesso,
> -- utilizei as funcoes criadas na question03

> question04 :: Client -> Validation
> question04 (Client xs ys zs )
>   | not(verifyTreeCaractere xs) && verifyDigit ys && verifyTreeCaractere zs && verifyEmailCaractere zs = Failure [NameLengthError]
>   | not(verifyLetter xs) && verifyDigit ys && verifyTreeCaractere zs && verifyEmailCaractere zs = Failure [NameCharactersError]
>   | verifyTreeCaractere xs && verifyLetter xs && not(verifyDigit ys) && verifyTreeCaractere zs && verifyEmailCaractere zs = Failure [PhoneError]
>   | verifyTreeCaractere xs && verifyLetter xs && verifyDigit ys && not(verifyTreeCaractere zs) = Failure [EmailSizeError]
>   | verifyTreeCaractere xs && verifyLetter xs && verifyDigit ys && not(verifyEmailCaractere zs) = Failure [EmailCharError]
>   | not(verifyTreeCaractere xs) && not(verifyDigit ys) && not(verifyTreeCaractere zs) = Failure [NameLengthError, PhoneError, EmailSizeError]
>   | otherwise = Ok

> question04Tests :: TestTree
> question04Tests
>       = testGroup "Question 04 Tests"
>                   [
>                      testCase "Valid client" $
>                        question04 (Client "Marcos" "123456789" "marcos@bla.com") @?= Ok
>                   ,  testCase "Invalid name - size" $
>                        question04 (Client "Mr" "123456789" "marcos@bla.com") @?= Failure [NameLengthError]
>                   ,  testCase "Invalid name - not all letters" $
>                        question04 (Client "Mr22" "123456789" "marcos@bla.com") @?= Failure [NameCharactersError]
>                   ,  testCase "Invalid phone" $
>                        question04 (Client "Marcos" "ab23" "marcos@bla.com") @?= Failure [PhoneError]
>                   ,  testCase "Invalid email - size" $
>                        question04 (Client "Marcos" "123456789" "m@") @?= Failure [EmailSizeError]
>                   ,  testCase "Invalid email - lacking @" $
>                        question04 (Client "Marcos" "123456789" "marcobla.com") @?= Failure [EmailCharError]
>                   ,  testCase "Combining errors" $
>                        question04 (Client "Mr" "aa" "b@") @?= Failure [NameLengthError, PhoneError, EmailSizeError]
>                   ]


Questão 05. Considere o seguinte tipo de dados que representa a configuração de uma
aplicação em um sistema gerenciador de janelas:

> data App
>       = App { name :: String  -- application name
>             , width :: Int    -- window width
>             , height :: Int   -- window height
>             }
>         deriving (Eq, Show)


Aplicações são organizadas de acordo com um layout:

> data Layout = Vertical [Layout]
>             | Horizontal [Layout]
>             | Single App
>             deriving (Eq, Show)

Neste gerenciador de janelas simples, aplicações são organizadas de maneira vertical
(construtor `Vertical`), horizontal (construtor `Horizontal`) ou uma janela simples.

Seu objetivo é implementar a função:

> --minimizeAll :: Layout -> Layout
> --minimizeAll = undefined

que minimiza todas as janelas do estado do gerenciador de janelas. Uma janela é
minimizada fazendo com que sua altura (height) e comprimento (width) sejam iguais a 1.

Sua implementação deve atender os seguintes casos de teste.

> -- Explicacao
> -- Como Layout Single App possui as informacoes height e width e as recursoes Vertical e Horizontal
> -- possuem Layout. E um Layout pode ser formado por apenas um Single ou por varios Vertical e Horizontal.
> -- Tendo como objetivo mencionado no enunciado realizar um update no valor de height e width para um, Pensei em percorrer
> -- as recursoes procurando o Single e realizar o update.

> minimizeAll :: Layout -> Layout
> minimizeAll (Single (App xx _ _)) = Single (App xx 1 1)
> minimizeAll (Horizontal ys) = Horizontal [minimizeAll y | y<-ys]
> minimizeAll (Vertical xs) = Vertical [minimizeAll x | x<-xs]

> question05Tests :: TestTree
> question05Tests
>       = testGroup "Question 05 Tests"
>                   [
>                       testCase "Minimize Single" $
>                         minimizeAll (Single (App "test" 110 200)) @?= Single (App "test" 1 1)
>                   ,   testCase "Minimize Vertical" $
>                         minimizeAll (Vertical [ Single (App "test" 110 200), Horizontal [Single (App "foo" 300 100)]])
>                               @?= Vertical [ Single (App "test" 1 1)
>                                            , Horizontal [Single (App "foo" 1 1)]]
>                   ,   testCase "Minimize Horizontal" $
>                         minimizeAll (Horizontal [ Single (App "test" 110 200)
>                                                 ,   Vertical [Single (App "foo" 300 100)]])
>                               @?= Horizontal [ Single (App "test" 1 1)
>                                              , Vertical [Single (App "foo" 1 1)]]
>                   ]
