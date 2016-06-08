{-# LANGUAGE TypeFamilies #-}

module Data.Graph.Class where

class Graph g where
  type Vertex g
  type Edges g

  edges :: g -> Vertex g -> Edges g

-- TODO: шардинг: нужно уметь определять какому воркеру
-- какие запросы слать.

-- Шардинг - разбиваем исходный массив на n равных частей, строим отображение
-- ключа в нужную часть.
