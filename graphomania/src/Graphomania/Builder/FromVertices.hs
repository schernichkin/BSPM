module Graphomania.Builder.FromVertices
  (

  ) where

-- TODO:
-- - Граф Fold по вершинам.
-- - Вершина. Внешний ID; Тэг (не испльзуется); Fold по рёбрам.
-- - Ребро. ID вершины, на которую оно указывает + Тэг.
-- ID вершины является внутренним идентификатором, но для вершины исходного графа.
-- 1. не использовать здесь внешние ID вершин.
--    они у нас будут храниться как тёги.
-- 2. Внутренние идентификаторы вершин меняются, как следствие мы строим
--    2 карты: ИД исход -> Ид целевая, Ид целевая -> ИД исход
