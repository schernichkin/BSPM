module Graphomania.Builder () where

-- TODO:
-- 1. Построение из триплетов vs построение из вершин.
--    - Построение из триплетов более универсально.
--    - При построении из вершин нам заранее известно кол-во ребер в вершине,
--      следовательно мы можем заранее выделить неолбходимое количество памяти.
--      Причем память можно выделить сразу на весь граф и на хеш-таблицы
--      (без значений).