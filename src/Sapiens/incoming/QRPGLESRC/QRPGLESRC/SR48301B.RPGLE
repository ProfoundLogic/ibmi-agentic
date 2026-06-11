      *  (C) Copyright Fiserv Insurance Solution Inc., 2006          *
     *
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
    H/TITLE SR48301B - ENDORSEMENT ATTACH/PRINT PROGRAM
     *===============================================================*
     * WC480301B - LIMITED OTHER STATES INSURANCE ENDORSEMENT        *
     *                                                               *
     * Program Description:                                          *
     *                                                               *
     *   Determines if this endorsement form should be attached to   *
     *   the policy and creates data in the Endorsement Form         *
     *   Workfile (WXFMP) prior to a form merge.                     *
     *                                                               *
     * Created   : 12/01/06                                          *
     * Programmer: C:184                                             *
     * Project   : WC1505                                            *
     *****************************************************************
     * REVISIONS:                                                    *
     *                                                               *
     * 11/18/08  C:61  SUPPORT WA135  65843                          *
     *                 only attach to WI coverage policies with
     *                 excluded states.                              *
     *                                                               *
     * 07/16/08  : PRISS04                                           *
     *             Valid routine should chain WDXSL to determine if  *
     *             policy has excluded state                         *
     *                                                               *
     * ------------------------------------------------------------- *
     *  NOTE: Please add comments to the top of revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     * Notes:                                                        *
     *                                                               *
     *  Utilizes the endorsement copybook (SCOPYEN01F/I/C).          *
     *                                                               *
     *  QQMODE = 1 - Called to determine if form should be attached. *
     *  QQMODE = 2 - Called to print endorsement.                    *
     *  QQMODE = 3 - Called for additional input.                    *
     *  QQMODE = 4 - Called for mid-term processing.                 *
     *===============================================================*
     *
     ** Copybook F specs
     F/COPY SCOPYEN01F
     *
     ** Additional Information File
     Fwxfmp1    if   e           k disk
     F                                     rename(pfm01:pfm02)
     F******** BEG ADD *** 07/16/08 ***********************************************************
     Fwdxsl     if   e           k disk
     F******** BEG ADD *** 07/16/08 ***********************************************************
     *
     *
     *
     *=========================================================================
     *  Definition Specs
     *=========================================================================
      *
     ** Copybook D specs
     D/COPY SCOPYEN01D
      *
      *
      *
     *=========================================================================
     *  /COPY required constants
     *=========================================================================
      *
     ** Form ID
     D qqform          c                   const('WC480301B ')
      *
     ** Additional entry program
     D addpgm          c                   const('SRxxxxxx  ')
      *
     D INM             S              2  0 INZ
      ** Names to include on form
     D                 ds
     D  nm                     1    160
     D                                     dim(16)
     D  FMC101                 1     10
     D  FMC102                11     20
     D  FMC103                21     30
     D  FMC104                31     40
     D  FMC105                41     50
     D  FMC106                51     60
     D  FMC107                61     70
     D  FMC108                71     80
     D  FMC109                81     90
     D  FMC110                91    100
     D  FMC111               101    110
     D  FMC112               111    120
     D  FMC113               121    130
     D  FMC114               131    140
     D  FMC115               141    150
     D  FMC116               151    160
     *=========================================================================
     *  PLIST definitions
     *=========================================================================
      *
     ** Entry parameters
      *
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqemp#            9
     C                   parm                    qqdiv             5
     C                   parm                    qqfyr             3
     C                   parm                    qqmode            1
     C                   parm                    qqrrn             9 0
     C                   parm                    qqmod2            1
      *
     *=========================================================================
     *  KLIST definitions
     *=========================================================================
     *
     ** Additional Input file
    
     ****** beg del ******* 11/18/08 *********************************
    C*****n1key         klist
    C**********         kfld                    xxco#
    C**********         kfld                    xxfnd
    C**********         kfld                    xxemp#
    C**********         kfld                    xxfyr
     ****** end del ******* 11/18/08 *********************************
     *
     C     XSky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxemp#
     C                   kfld                    xxfyr
    
     *=========================================================================
     *  Main
     *=========================================================================
     ** Copybook C specs
     C/COPY SCOPYEN01C
    
     *
     *=========================================================================
     *  OTHINF - Subroutine to load form specific information & write record
     *=========================================================================
     C     othinf        begsr
     C
     C                   Z-ADD     0             X                 2 0
     C     xsky          setll     Pxs01
     C     xsky          reade     Pxs01                                  60
     C
     C                   dow       *in60 = *off
     C                   eval      inm= inm+ 1
     C                   movel     Xsst          nm(inm)
     ** Write the output record
     C                   If        inm= 16
     C                   write     pfm01
     C                   clear                   nm
     C                   eval      inm= *zero
     C                   endif
     C     xsky          reade     pxs01                                  60
     C
     C                   Enddo
     ** Write the output record
     C                   If        inm < 16
     C                   write     pfm01
     C                   clear                   nm
     C                   eval      inm= *zero
     C
     C                   endif
     *
     C                   endsr
     *
     *=========================================================================
     *  PGMSTR - Beginning of program processing
     *=========================================================================
     C     pgmstr        begsr
     C                   endsr
     *
     *
     *
     *=========================================================================
     *  PGMEND - End of program processing
     *=========================================================================
     C     pgmend        begsr
     C                   endsr
     *
     *
     *
     *=========================================================================
     *  VALID - Subroutine to attach this form to the policy
     *=========================================================================
     C     valid         begsr
     *
     ****** beg del ******* 11/18/08 *********************************
    C**********         IF        XSST  = 'WI'
    C*****rsky*         chain     prs01
    C**********         if        %found
    C**********                   and rsjrst = enst
    C**********         endif
    C**********         endif
    C*****xsky*         chain     pxs01
    C**********         if        %found
    C**********         move      *on           crtfrm
    C**********         endif
     ****** end del ******* 11/18/08 *********************************
     ****** beg add ******* 11/18/08 *********************************
      *
     C     rsky          chain     prs01
      *
     C                   if        %found
     C                             and rsjrst = 'WI'
     C     xsky          chain     pxs01
      *
     C                   if        %found
     C                   move      *on           crtfrm
     C                   endif
      *
     C                   endif
      *
     ****** end add ******* 11/18/08 *********************************
     *
     C                   endsr
     *
     *
