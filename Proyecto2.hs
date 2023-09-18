{-
Algoritmos y Estructuras de Datos I 
Laboratorio Proyecto 2

Ejercicios
-}

--1) Tipos Enumerados
--a)
data Carrera = Matematica | Fisica | Computacion | Astronomia

--b)
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matemática"
titulo Fisica = "Licenciatura en Física"
titulo Computacion = "Licenciatura en Computación"
titulo Astronomia = "Licenciatura en Astronomía"

{-
> titulo Matematica
> "Licenciatura en Matemática"

> titulo Computacion
> "Licenciatura en Computación"
-}

--c)
data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Eq, Ord, Show, Bounded)

--d)
cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

{-
> cifradoAmericano Re
> 'D'

> cifradoAmericano Fa
> 'F'
-}

--3) Polimorifismo ad hoc
--a)
minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

{-
> minimoElemento [Re, Mi, Fa, Do]
> Do

> minimoElemento [20..40]
> 20
-}

--b)
minimoElemento' ::(Bounded a, Ord a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)

{-
> minimoElemento' ([Sol, Re, Mi, Fa]::[NotaBasica])
> Re

> minimoElemento' ([] :: [Int])
> 9223372036854775807
-}

--c)
notaGrave :: NotaBasica
notaGrave = minimoElemento [Fa, La, Sol, Re, Fa] --return: Re

--4) Sinónimo de Tipos

--a)

-- Enum
data Zona = Arco | Defensa | MedioCampo | Delantera deriving (Eq, Show)
data TipoReves = DosManos | UnaMano deriving Show
data Modalidad = Carretera | Pista | Monte | BMX deriving Show
data PiernaHabil = Izquierda | Derecha deriving Show

-- Alias
type Altura = Int
type NumCamiseta = Int
type ManoHabil = PiernaHabil

-- Tipo Algebraico
data Deportista = Ajedrecista
  | Ciclista Modalidad
  | Velocista Altura
  | Tenista TipoReves ManoHabil Altura
  | Futbolista Zona NumCamiseta PiernaHabil Altura deriving Show

--b) Ciclista :: Modalidad -> Deportista

--c)
contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas ((Velocista h):xs) = 1 + contar_velocistas xs
contar_velocistas (_:xs) = contar_velocistas xs

{-
> contar_velocistas [Velocista 2, Ciclista Monte, Velocista 5]
> 2

> contar_velocistas [Ajedrecista, Ciclista BMX, Ciclista Monte, Velocista 5]
> 1
-}

--d)
contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] _ = 0
contar_futbolistas ((Futbolista z' n p h):xs) z 
  | z == z' = 1 + contar_futbolistas xs z
  | otherwise = contar_futbolistas xs z
contar_futbolistas (_:xs) z = contar_futbolistas xs z

{-
> contar_futbolistas [Ajedrecista, Velocista 1, Futbolista Arco 2 Derecha 5, Futbolista Delantera 10 Derecha 5] Arco
> 1

> contar_futbolistas [Futbolista Delantera 8 Derecha 10, Futbolista Arco 2 Derecha 5, Futbolista Delantera 10 Derecha 5] Delantera
> 2
-}

--e)
esFutbolistaEnZ :: Zona -> Deportista -> Bool
esFutbolistaEnZ z (Futbolista z' n p h) = z'==z
esFutbolistaEnZ z _ = False

contar_futbolistas' :: [Deportista] -> Zona -> Int
contar_futbolistas' [] _ = 0
contar_futbolistas' xs z = length (filter (esFutbolistaEnZ z) xs)

{-
> contar_futbolistas' [Tenista UnaMano Derecha 5, Velocista 1, Futbolista Arco 2 Derecha 5, Futbolista Delantera 10 Derecha 5] Arco
> 1

> contar_futbolistas' [Futbolista Delantera 8 Derecha 10, Ajedrecista, Futbolista Delantera 10 Derecha 5] Delantera
> 2
-}

--5) Definición de Clases
--a)
sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11

{-
> sonidoNatural Re
> 2

> sonidoNatural Fa
> 5
-}

--b)
data Alteracion = Bemol | Natural | Sostenido deriving Eq

--c)
data NotaMusical = Nota NotaBasica Alteracion

--d)
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota x Sostenido) = sonidoNatural x + 1
sonidoCromatico (Nota x Bemol) = sonidoNatural x - 1
sonidoCromatico (Nota x Natural) = sonidoNatural x 

{-
> sonidoCromatico (Nota Re Sostenido)
> 3

> sonidoCromatico (Nota Fa Bemol)
> 4
-}

--e)
instance Eq NotaMusical where
    x == y = sonidoCromatico x == sonidoCromatico y

{-
> Nota Mi Sostenido == Nota Fa Bemol
> False

> Nota Mi Natural == Nota Fa Bemol
> True
-}

--f)
instance Ord NotaMusical where
    compare x y = compare (sonidoCromatico x) (sonidoCromatico y)

{-
> Nota Mi Natural > Nota Fa Bemol
> False

> Nota Mi Natural < Nota Fa Sostenido
> True
-}

--6) Tipos Enumerados con Polimorfismo
--a)
primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x

{-
> primerElemento []
> Nothing

> primerElemento [2,34,5]
> Just 2
-}

--7) Tipos Recursivos
data Cola = VaciaC | Encolada Deportista Cola deriving Show

--a)
--1)
atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ c) = Just c

{-
> atender (Encolada Ajedrecista (Encolada (Velocista 40) VaciaC))
> Just (Encolada (Velocista 40) VaciaC)

> atender VaciaC
> Nothing
-}

--2)
encolar :: Deportista -> Cola -> Cola
encolar d VaciaC = Encolada d VaciaC
encolar d (Encolada d' c) = Encolada d' (encolar d c)

{-
> encolar (Velocista 5) (Encolada Ajedrecista (Encolada (Velocista 40) VaciaC))
> Encolada Ajedrecista (Encolada (Velocista 40) (Encolada (Velocista 5) VaciaC))

> encolar (Velocista 5) VaciaC
> Encolada (Velocista 5) VaciaC
-}

--3) 
busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC _ = Nothing
busca (Encolada d c) z
  | esFutbolistaEnZ z d = Just d
  | otherwise = busca c z

{-
> busca (Encolada Ajedrecista (Encolada (Futbolista Arco 2 Derecha 5) VaciaC)) Delantera
> Nothing

> busca (Encolada Ajedrecista (Encolada (Futbolista Arco 2 Derecha 5) VaciaC)) Arco
> Just (Futbolista Arco 2 Derecha 5)
-}

--b) Se parece al Tipo String "data String = VacioS | Cadena Char String" que es 'Tipo Recursivo'

--8) Tipos Recursivos y Polimórficos
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving Show

type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

--a)
type GuiaTelefonica = ListaAsoc String Int

--b)
--1)
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo _ _ abs) = 1 + la_long abs

{-
> la_long ((Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 Vacia))::GuiaTelefonica)
> 2 

> la_long ((Nodo 45223141 "Ramirez" (Nodo 23151515 "Gonzalez" Vacia))::Padron)
> 2
-}

--2)
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia l' = l'
la_concat l Vacia = l
la_concat (Nodo a b abs) l' = Nodo a b (la_concat abs l')

{-
> la_concat ((Nodo 45223141 "Ramirez" (Nodo 23151515 "Gonzalez" Vacia))::Padron) ((Nodo 45223231 "Kuttel" (Nodo 53151515 "Guti" Vacia))::Padron)
> Nodo 45223141 "Ramirez" (Nodo 23151515 "Gonzalez" (Nodo 45223231 "Kuttel" (Nodo 53151515 "Guti" Vacia)))

> la_concat ((Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 Vacia))::GuiaTelefonica) ((Nodo "Jewel" 55223141 (Nodo "Kuttel" 231514515 Vacia))::GuiaTelefonica)
> Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 (Nodo "Jewel" 55223141 (Nodo "Kuttel" 231514515 Vacia)))
-}

--3)
la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia a' b' = Nodo a' b' Vacia
la_agregar (Nodo a b abs) a' b'
  | a == a' = Nodo a b' abs
  | otherwise = Nodo a b (la_agregar abs a' b')

{-
> la_agregar ((Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 Vacia))::GuiaTelefonica) "Rofi" 32512515
> Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 (Nodo "Rofi" 32512515 Vacia))

> la_agregar ((Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 Vacia))::GuiaTelefonica) "Rodriguez" 22222222
> Nodo "Rodriguez" 22222222 (Nodo "Cervera" 23151515 Vacia)
-}

--4)
la_pares :: ListaAsoc a b -> [(a,b)]
la_pares Vacia = []
la_pares (Nodo a b abs) = (a,b) : la_pares abs

{-
> la_pares ((Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 Vacia))::GuiaTelefonica)
> [("Rodriguez",45223141),("Cervera",23151515)]

> la_pares ((Nodo 45223141 "Ramirez" (Nodo 23151515 "Gonzalez" Vacia))::Padron)
> [(45223141,"Ramirez"),(23151515,"Gonzalez")]
-}

--5)
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing
la_busca (Nodo a b abs) a'
  | a == a' = Just b
  | otherwise = la_busca abs a'

{-
> la_busca ((Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 Vacia))::GuiaTelefonica) "Cervera"
> Just 23151515

> la_busca ((Nodo "Rodriguez" 45223141 (Nodo "Cervera" 23151515 Vacia))::GuiaTelefonica) "Hernandez"
> Nothing
-}

--6)
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia
la_borrar a' (Nodo a b abs)
  | a' == a = la_borrar a' abs
  | otherwise = Nodo a b (la_borrar a' abs)

{-
> la_borrar "Perez" ((Nodo "Rodriguez" 45223141 (Nodo "Baigorria" 23151515 (Nodo "Perez" 2134142 Vacia)))::GuiaTelefonica)
> Nodo "Rodriguez" 45223141 (Nodo "Baigorria" 23151515 Vacia)

> la_borrar "Hernandez" ((Nodo "Rodriguez" 45223141 (Nodo "Baigorria" 23151515 (Nodo "Perez" 2134142 Vacia)))::GuiaTelefonica)
> Nodo "Rodriguez" 45223141 (Nodo "Baigorria" 23151515 (Nodo "Perez" 2134142 Vacia))
-}
