     *===============================================================*
     *  (C) Copyright Progressive Data Solutions, Inc., 1999         *
     *===============================================================*
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SR21307 - WC210307 - MI SPECIFIC PERSON EXCLUSION ENDORSEMENT
     *===============================================================*
     * WC210307 - Michigan Specific Person Exclusion Endorsement     *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 08/25/99                                          *
     * Programmer: C:113                                             *
     * Project   : 96WCM                                             *
     *===============================================================*
     * REVISIONS:                                                    *
      *
      * 07/14/11 C:061 Support 77336
      *              - Convert source to RPGIV.
      *              - Do not attach for flat cancelled divisions.
     *                                                               *
      * 05/25/01 C:90  PROJECT: CM027                                 *
      *                Load proper fields for endorsement.            *
      *                                                               *
     * XX/XX/XX C:XXX PROJECT: XXXXX                                 *
     *                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS            *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *  This is a manual attach endorsement with additional input.   *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *                                                               *
     *===============================================================*
     ** Copybook F specs
    F/COPY SCOPYEN01F
      *
     FWMOOL     IF   E           K DISK
     FWXFMP1    IF   E           K DISK
     F                                     RENAME(PFM01:PFM02)
     *=========================================================================
     *  Arrays
     *=========================================================================
      *
     ** Owner/Officer names
     D**** BEGIN CHG **** 05/25/01 ************************************
     D******************* NM         10 40
     D**** END   CHG **** 05/25/01 ************************************
      *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
     ** Form ID
     D qqFORM          C                   CONST('WC210307  ')
      ******* beg del ****** 07/14/11 *********************************
      ******* Additional entry program
     D******* ADDPGM          C                   CONST('          ')
      ******* Copybook I specs
     D*******/COPY SCOPYEN01I
      ******* end del ****** 07/14/11 *********************************
      *
     *=========================================================================
     *  *LIKE DEFN definitions
     *=========================================================================
      *
     ** Owner/Officer names
     D                 DS
     D**** BEGIN CHG **** 05/25/01 ************************************
     D*************************************   1 400 NM
     D  NM                     1    200
     D                                     DIM(5)
     D  FMC401                 1     40
     D  FMC402                41     80
     D  FMC403                81    120
     D  FMC404               121    160
     D  FMC405               161    200
     I************************************* 201 240 FMC406
     I************************************* 241 280 FMC407
     I************************************* 281 320 FMC408
     I************************************* 321 360 FMC409
     I************************************* 361 400 FMC410
     I**** END   CHG **** 05/25/01 ************************************
      ******* beg add ****** 07/14/11 *********************************

      ** Additional entry program
     D ADDPGM          C                   CONST('SRXXXXXXX ')

     D/COPY SCOPYEN01D


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     ** Entry parameters
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
    
      ******* end add ****** 07/14/11 *********************************
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
      *
     ** Additional Input file
     C     F3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    xxFORM
      *
     ** Owner/Officer file
     C     OOKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    ELDIV
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
    C/COPY SCOPYEN01C
      *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     OTHINF        BEGSR
      *
     C     F3KY          CHAIN     WXFMP1                             60
     ** Move the additional input fields
     C                   MOVE      F3FL01        FMFL01
     C                   MOVE      F3FL02        FMFL02
     C                   MOVE      F3FL03        FMFL03
     C**** BEGIN ADD **** 05/25/01 ************************************
     C                   MOVE      F3FL04        FMFL04
     C**** END   ADD **** 05/25/01 ************************************
     C                   MOVEL     F3NM01        FMNM01
     C                   MOVEL     F3NM02        FMNM02
     C                   MOVEL     F3NM03        FMNM03
     C                   MOVEL     F3NM04        FMNM04
     C                   MOVEL     F3NM05        FMNM05
     C                   MOVEL     F3NM06        FMNM06
     C                   MOVEL     F3NM07        FMNM07
     C                   MOVEL     F3NM08        FMNM08
     C                   MOVEL     F3NM09        FMNM09
     C                   MOVEL     F3NM10        FMNM10
     C**** BEGIN ADD **** 05/25/01 ************************************
     C                   MOVEL     F3NM11        FMNM11
     C                   MOVEL     F3NM12        FMNM12
     C                   MOVEL     F3NM13        FMNM13
     C                   MOVEL     F3NM14        FMNM14
     C                   MOVEL     F3C401        FMC401
     C**** END   ADD **** 05/25/01 ************************************
      *
     C                   Z-ADD     *ZERO         IX                3 0
     C                   Z-ADD     *ZERO         JX                3 0
      *
     C     ELKY2         CHAIN     PEL02                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      EMTPOB        FMTOBC
      *
     ** Look for a qualifying "EXCLUDED" Owner/Officer record.
     C     OOKY          SETLL     POO01
     C     OOKY          READE     POO01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     OOEXFL        IFEQ      'E'                                          EXCLUDED
     C     OOELID        ANDEQ     *BLANK                                       NOT ENT/LOC
     C     OOSTAT        ANDNE     'I'                                          NOT INACTIVE
      *
     C     OOEFDT        IFLE      ELPTDT                                       VALID WITHIN
      *
     C     OOEXDT        IFEQ      *ZERO                                        POLICY DATES
     C     OOEXDT        ORGT      ELPFDT
      *
     C     OOOFSN        CAT(P)    OOOLSN:1      XXNM40           40
     C                   Z-ADD     1             X                 2 0
     C     XXNM40        LOOKUP    NM(X)                                  65
     C     *IN65         IFEQ      '0'
     C                   ADD       1             IX
     C                   ADD       1             JX
     C                   MOVEL     XXNM40        NM(IX)
      *
     ** If the page is full, write a record and set up for more names.
     C     IX            IFEQ      10
     C                   WRITE     PFM01
     C                   MOVE      *BLANK        NM
     C                   Z-ADD     *ZERO         IX
     C                   ENDIF
     C                   ENDIF                                                  *IN65='0'
     C                   ENDIF                                                  OOEXDT =  *ZERO
     C                   ENDIF                                                  OOEFDT <= ELPTDT
     C                   ENDIF                                                  OOEXFL = 'E'
      *
     C     OOKY          READE     POO01                                  60
     C                   ENDDO
      *
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO
      *
     ** Write the output record.
     C     IX            IFNE      *ZERO
     C     JX            OREQ      *ZERO
     C                   WRITE     PFM01
     C                   ENDIF
      *
     C                   ENDSR
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     PGMSTR        BEGSR
      *
     C     F3KY          CHAIN     WXFMP1                             60
      *
      *
     C                   ENDSR                                                  PGMSTR
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     PGMEND        BEGSR
      *
      *
     C                   ENDSR                                                  PGMEND
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     VALID         BEGSR
      *
     ** This is a manual attach endorsement, so set on CRTFRM.
     C                   MOVE      *ON           CRTFRM
      *
     C                   ENDSR                                                  VALID
