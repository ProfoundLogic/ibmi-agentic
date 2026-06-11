    H/TITLE IS830R - Update Index Entries File
      *
     *  REVISIONS:
      *  05/06/09     C:180      SUPPORT 66811
      *
     Fimiep     uf   e             disk    usropn
     D OLDSTS          S              2    INZ('SC')
     D NEWSTS          S              2    INZ('RL')
      *
     C                   OPEN      IMIEP
     C/exec sql
     C+ UPDATE IMIEP SET IEDSTS = :NEWSTS WHERE IEDSTS=:OLDSTS
     C/end-exec
     C                   CLOSE     IMIEP
     C                   EVAL      *INLR = *ON
