module QueryTests where

import GitParser
import VPMNewTest
import Main

--check parsing

-- Queries run for the thesis example
--1. ./Main "ms from ms <- match \"public\" in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java "
--2. ./Main "d from _ <- match d<\"curr\",\"currDesig\"> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java "
--3. ./Main "m from m <- match #10<_,_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java " 
--4. ./Main "x,d from _ <- match d<\"getAllDesignations()\",x> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java "
--5.
--6.
--7. ./Main "m from m <- match d<,x> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java"


--test
-- ./Main "x from m <- match d<x,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java"


--Conditions
-- ./Main "x from m <- match d<x,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java where d.date == \"2017/11/23\" "
-- ./Main "x from m <- match d<x,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java where (d.date >= \"2017/11/19\" or d.date == \"2017/11/21\")
-- ./Main "x from m <- match d<x,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java where (d.date >= \"2017/11/19\" and d.date <= \"2017/11/21\") "
-- ./Main "x from m <- match d<x,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java where d.author == \"Deepthi S Kumar\""
-- ./Main "x from m <- match d<x,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java,  m' <- match d1<y,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java where x == y"
-- ./Main "y from m <- match d<x,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java,  m1 <- match d1<y,_\"getCurrentDesignation()\"_> in -f //home/eecs/Documents/gitRepos/thesisExample2/Employee.java where x /= y"
