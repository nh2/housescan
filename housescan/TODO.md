TODO
====

- Currently all matrices are right-multiplied.
  Maybe change my convention, but then better also write my own Proj4 module

- This segfaulted once in ghci:
  `forM_ [projTest, projTest2, projTest3, projTest4, projTest5, projTest6] (\f -> run f >> run clearRooms)`
  But I couldn't reproduce it straight after.
