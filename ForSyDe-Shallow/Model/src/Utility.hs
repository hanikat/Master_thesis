--------------------------------------------------
-- Project: Master thesis - ForSyDe model
-- Created by: Marcus Hanikat
-- Created: Mon Mar 1 2021
-- Contact: hanikat@kth.se
-- Copyright: (c) 2021 Marcus Hanikat
--
-- Description: Utility functions used throughout model
--------------------------------------------------
module Utility where

-- Helper functions for parsing a 4-way tuple
first :: (Num a) => (a,a,a,a) -> a 
first (x,_,_,_) = x

second :: (Num a) => (a,a,a,a) -> a 
second (_,x,_,_) = x

third :: (Num a) => (a,a,a,a) -> a 
third (_,_,x,_) = x

fourth :: (Num a) => (a,a,a,a) -> a 
fourth (_,_,_,x) = x