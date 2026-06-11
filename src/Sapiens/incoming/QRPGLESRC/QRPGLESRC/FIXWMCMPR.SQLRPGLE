     FWMCMP     uf   e             disk    usropn
     C                   OPEN      WMCMP
     C/exec sql
     C+ UPDATE WMCMP SET CMRLW = CMRLWK, CMRLWK = 0 WHERE CMRLW = 0 AND
     C+ CMRLWK <> 0
     C/end-exec
      *
     C/exec sql
     C+ UPDATE WMCMP SET CMRLWK = 0 WHERE CMRLW = CMRLWK AND
     C+ CMRLWK <> 0
     C/end-exec
     C                   CLOSE     WMCMP
     C                   EVAL      *INLR =*ON
