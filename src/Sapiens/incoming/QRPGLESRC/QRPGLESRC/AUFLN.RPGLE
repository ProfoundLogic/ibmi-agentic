
     C/EJECT
     C*** THIS ROUTINE WILL COMBINE THE TWO VALUES PASSED INTO
     C*** ONE TEXT VARIABLE, SEPARATING THEM WITH A COMMA.
     C*** THE FOLLOWING FIELDS AND ARRAYS ARE USED BY THIS
     C*** ROUTINE.      XXLNAM (20)-FIRST SECTION OF RESULT
     C***               XXFNAM (15)-SECOND SECTION OF RESULT
     C***               XXCNAM (42)-RESULT FIELD
     C***               XXCN   (42 X 1)-RESULT ARRAY
     C***
     C***
     C     AUFLN         BEGSR
     C*
     C                   MOVE      ' '           XXCN
     C*
     C                   MOVE      '0'           XX60              1
     C   60              MOVE      '1'           XX60
     C*
     C*** MOVE THE FIRST PARM INTO THE RESULT FIELD AND EST-
     C*** ABLISH THE INDEX OF THE FIRST NON-BLANK STARTING
     C*** FROM THE RIGHT MOST BYTE OF THE FIRST PARM.
     C*
     C                   MOVEL     XXLNAM        XXCNAM
     C*
     C                   Z-ADD     20            Z                 2 0
     C     AUFLN1        TAG
     C     XXCN(Z)       COMP      ' '                                    60
     C  N60              GOTO      AUFLN2
     C                   SUB       1             Z                    60
     C   60              GOTO      AUFLN1
     C*
     C*** IF THE FIRST PARAMETER IS A BLANK SIMPLY MOVE THE
     C*** SECOND PARM INTO THE RESULT FIELD STARTING WITH
     C*** THE RIGHT MOST BYTE.
     C*
     C                   MOVEL     XXFNAM        XXCNAM
     C                   GOTO      AUFLN3
     C*
     C*** USING THE INDEX ESTABLISHED IN THE FIRST SECTION OF
     C*** THIS ROUTINE (Z), PLACE THE COMA APPROPRIATELY AS
     C*** WELL AS THE SECOND PARAMETER.
     C***
     C*
     C     AUFLN2        TAG
     C                   ADD       1             Z
     C                   MOVE      ','           XXCN(Z)
     C*
     C                   ADD       2             Z
     C                   MOVEA     XXFNAM        XXCN(Z)
     C*
     C     AUFLN3        TAG
     C*
     C*** RESET THE INDICATORS USED IN THIS ROUTINE.
     C*
     C     XX60          COMP      '1'                                    60
     C                   ENDSR
     C/SPACE 3
