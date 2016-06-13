{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}
{-# LANGUAGE UnboxedTuples       #-}

module Offheap.GetBSS () where

--TODO: Исследовать возможность работы со статической информацией о размере
-- читаемых данных + стрелочный интерфейс. Более общий вариант: инкапсулировать
-- в стрелку быстрый статический ридед и медленный динамический.

import           Control.Arrow
import           Control.Category
import           Data.Int
import           Data.Primitive
import           GHC.Prim
import           GHC.Types
import           Prelude          hiding (id, (.))

data GetBSS a b = GetBSS Static (Dynamic a b)

data Static = Static
  { _size :: Int
  }

newtype Dynamic a b = Dynamic { unDynamic :: (# a, Addr# #) -> (# b, Addr# #) }

instance Category GetBSS where
  id = GetBSS (Static 0) (Dynamic $ \x -> x )
  (GetBSS (Static i) (Dynamic f)) . (GetBSS (Static j) (Dynamic g)) =
    GetBSS (Static (i + j)) (Dynamic $ \x -> f (g x))

instance Arrow GetBSS where
  arr f = GetBSS (Static 0) . Dynamic $ \(# x, addr #) -> (# f x, addr #)
  first (GetBSS s (Dynamic f)) = GetBSS s $ Dynamic $ \(# (a, b), addr #) ->
    let (# c, addr' #) = f (# a, addr #)
    in (# (c, b), addr' #)

{-# INLINE getPrim #-}
getPrim :: forall a b . (Prim b) => GetBSS a b
getPrim = GetBSS (Static (I# size)) $ Dynamic $ \(# _, addr #) ->
                 (# indexOffAddr# addr 0#
                 , plusAddr# addr size #)
  where
    size = sizeOf# (undefined :: b)

{-# INLINE getInt16Host #-}
getInt16Host :: GetBSS a Int16
getInt16Host = getPrim

{-# INLINE getInt32Host #-}
getInt32Host :: GetBSS a Int32
getInt32Host = getPrim

{-# INLINE getInt64Host #-}
getInt64Host :: GetBSS a Int64
getInt64Host = getPrim

-- Композиция динамических и статических парсеров.
-- 1. Статические парсеры.
--    - Композиция статических парсеров является статическим парсером.
--    - Статические парсеры не проверяют выход за диапазон в процессе парсинга,
--      эта проверка выполняется до начала парсинга.
--    - Статические парсеры могут быть использованы для создания массивов.
-- 2. Динамические парсеры.
--    - Композиция динамических парсеров является динамическим парсером.
--    - Динамические парсеры не проверяют выход за диапазон перед началом
--      парсинга, проверка выполняется динамически в процессе парсинга.
--    - Динамические парсеры не могут быть использованы для создания массивов.
-- 3. Смешанные парсеры.
--    - Композиция статического и динамического парсера является динамическим
--      парсером
--    - Перед вызовом статической части необходимо выполнить проверку
--    - Переб вызовом динамической части проверку выполнять не надо.
-- 4. Оптимизация
--    - d . s . s менее выгодна, чем d . (s . s), возможно следует создать
--      правила перезаписи для выбора более выгодной композиции?
--    - придумать как выполнять лифтинг проверок для статических парсеров
--      (собственно, с этого надо начать)
--    Замечания:
--      - Лифтинг проверок программный: композиция статических парсеров
--        не добавляет проверок, проверки вставляются при композиции
--        статического парсера с динамическим, либо на корневом уровне.
--      - Для применения перезаписи нужно статически знать тип парсера,
--        судя по всему правила перезаписи могут оперировать с константами
--        надо проверить, действительно ли это так (как минимум требуется
--        поддержка частичного вычисления на этапе компиляции), если это не
--        так, надо рассмотреть вариант с GADT, кодирующим тип парсера.
