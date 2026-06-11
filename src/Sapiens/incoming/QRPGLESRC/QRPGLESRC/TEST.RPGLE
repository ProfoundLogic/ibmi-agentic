     H*****************************************************************
     H*
     H* PROGRAM: CHANGUSPTR
     H*
     H* LANGUAGE: ILE RPG for OS/400
     H*
     H* DESCRIPTION: CHANGE THE CONTENTS OF INFORMATION IN THE USER
     H*              AREA IN THE USER SPACE USING A POINTER
     H*
     H*****************************************************************
     D*
     DUSRSPCNAM        S             20    INZ('TEMPSPACE QTEMP     ')
     DNEWVALUE         S             64    INZ('Big String padded with blanks')
     DUSRSPCPTR        S               *
     DUSERAREA         DS                  BASED(USRSPCPTR)
     D CHARFIELD               1     64
     D*
     D/COPY QUSEC
     D* Following QUSEC structure copied from QSYSINC library
     D*QUSEC            DS
     D*                                             Qus EC
    DD* QUSBPRV                 1      4B 0
     D*                                             Bytes Provided
     D* QUSBAVL                 5      8B 0
     D*                                             Bytes Available
     D* QUSEI                   9     15
     D*                                             Exception Id
     D* QUSERVED               16     16
     D*                                             Reserved
     C*
     C* Initialize Error code structure to return error ids
     C*
     C                   Z-ADD     16            QUSBPRV
     C*
     C* Set USRSPCPTR to the address of the User Space
     C*
     C                   CALL      'QUSPTRUS'
     C                   PARM                    USRSPCNAM
     C                   PARM                    USRSPCPTR
    CC                   PARM                    QUSEC
     C* Check for successful setting of pointer
     C*
     C     QUSBAVL       IFGT      0
     C*
     C* If an error, then display the error message id
     C*
     C                   DSPLY                   QUSEI
     C                   ELSE
     C*
     C* Otherwise, update the User Space via the based structure
     C                   MOVEL     NEWVALUE      USERAREA
     C                   END
     C*
     C* And return to our caller
     C*
     C                   SETON                                        LR
     C                   RETURN
