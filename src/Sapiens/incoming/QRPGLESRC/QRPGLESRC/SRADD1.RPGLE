     Hcopyright('(c) 2025 Sapiens, Inc. All rights reserved. -
     H Sapiens is a trademark of Sapiens, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     *
    H/TITLE SRADD1 - Retreive PEO Name Information
     *===============================================================*
      *  GIVEN:   co/fnd/emp/div/fyr
      *  RETURN:  Name, Federal Name, address info, FEIN
      *
     *===============================================================*
     * Revisions:                                                    *
     H*                                                               *
     H* 07/14/25 C:65  Project: 117417 (AmWins)                       *
     H*                Remove the concatenation of DBA for Amwins     *
     H*                as they always input the characters into name. *
     H*                                                               *
     H* 04/05/24 C:300 Project: 113842 (AmWins)                       *
     H*                If exists, return the Federal name of PEO      *
     H*                instead of the DBA name                        *
     H*                                                               *
     H* 01/15/18 C:65  Project: 150WCJ                                *
     H*                If a policy has an overriding PEO specified    *
     H*                then pull the PEO name from the indicated      *
     H*                additional named insured record.               *
     *                                                               *
     * 03/28/17 C:65  Project: SY371                                 *
     *                Return PEO fein for output mode of '3'.        *
     *                                                               *
     * 03/22/11 C:65  project Supp#75755                             *
     *                Change name on output conrol flag '3' or blank *
     *                to be PEO LCF Client Fed DBA Client Dba name.  *
     *                                                               *
     * 11/15/06 C:61  project 139WCI                                 *
     *                Created.
     *                                                               *
     *===============================================================*

      * Group
     Fwmfnl     if   e           k disk

      * Employer Master
     Fwmeml     if   e           k disk

      * Employer Policy Years
     Fwdell     if   e           k disk

      * Rating State Code
     Fwmrsl     if   e           k disk

      * Reporting/Rating
     Fwdrrl     if   e           k disk
     F*** begin add ***  01/15/18  **************************************
     FWMANL1    IF   E           K DISK
     F*** end add *****  01/15/18  **************************************


     *=========================================================================
     *  Definition Specs
     *=========================================================================

     d PEO_name        s            100
     d PEO_fnam        s            100
     d PEO_ead1        s             50
     d PEO_ead2        s             50
     d PEO_cty         s             25
     d PEO_st          s              2
     d PEO_zip         s              9  0
     d PEO_fein        s              9  0

     d CLT_name        s            100
     d CLT_fnam        s            100
     d CLT_ead1        s             50
     d CLT_ead2        s             50
     d CLT_cty         s             25
     d CLT_st          s              2
     d CLT_zip         s              9  0
     d CLT_fein        s              9  0


     *=========================================================================
     *  KLIST definitions
     *=========================================================================

     * Group

     c     fnky          klist
     c                   kfld                    qqco#
     c                   kfld                    qqfnd

     * Employer

     c     emky          klist
     c                   kfld                    qqco#
     c                   kfld                    qqfnd
     c                   kfld                    qqemp#
     c                   kfld                    qqdiv

     c     emky1         klist
     c                   kfld                    qqco#
     c                   kfld                    qqfnd
     c                   kfld                    xxemp#            9 0
     c                   kfld                    xxdiv             5 0

     * Employer Policy Years

     c     elky          klist
     c                   kfld                    qqco#
     c                   kfld                    qqfnd
     c                   kfld                    qqemp#
     c                   kfld                    qqdiv
     c                   kfld                    qqfyr

     * Rate Set Code file

     c     rsky          KList
     c                   KFld                    qqco#
     c                   KFld                    elrtcd

     * Reporting Rating

     c     rrky          klist
     c                   kfld                    qqco#
     c                   kfld                    qqfnd
     c                   kfld                    rsjrst
     c                   kfld                    elpfdt
      *
     c     rrky1         klist
     c                   kfld                    qqco#
     c                   kfld                    qqfnd
     c                   kfld                    rsjrst
     C*** begin add ***  01/15/18  **************************************
     C*
     C*  Additional Named Insured File
     C*
     C     ANKY          KLIST
     C                   KFLD                    qqCO#
     C                   KFLD                    qqFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXSEQ#            5 0
     C*** end add *****  01/15/18  **************************************


     *=========================================================================
     *  PLIST definitions
     *=========================================================================

     * Entry

     c     *entry        plist
     c                   parm                    qqco#             3 0
     c                   parm                    qqfnd             3 0
     c                   parm                    qqemp#            9 0
     c                   parm                    qqdiv             5 0
     c                   parm                    qqfyr             3 0
     c                   parm                    qqname          100
     c                   parm                    qqfnam          100
     c                   parm                    qqead1           40
     c                   parm                    qqead2           40
     c                   parm                    qqecsz           40
     c                   parm                    qqcty            25
     c                   parm                    qqst              2
     c                   parm                    qqzip             9 0
     c                   parm                    qqfein            9 0
     c                   parm                    qqeadr           90

     c     linepl        plist
     c                   parm                    ##line          256
     c                   parm                    ##length          3 0
     c                   parm                    ##remain        256


     *=========================================================================
     *  Main
     *=========================================================================

      * Initialize

     c                   clear                   qqname
     c                   clear                   qqfnam
     c                   clear                   qqead1
     c                   clear                   qqead2
     c                   clear                   qqcty
     c                   clear                   qqst
     c                   clear                   qqzip
     c                   clear                   qqfein
     C*** begin add ***  07/14/25  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C*** end add *****  07/14/25  **************************************
     C*** begin add ***  01/15/18  **************************************
     C     EMKY          CHAIN     PEM01                              60
     C                   Z-add     EMPEONMIN     xxSeq#
     C*** end add *****  01/15/18  **************************************

      * Get PEO info
     c                   exsr      getPEO

      * Get client info
     c                   exsr      getCLT
    
      * Get Group info - PEO Flag
    
     c     fnky          chain     pfn01

     c                   if        %found

      * Get Reporting/Rating info - PEO Output Control Flag

     c     elky          chain     pel01

     c                   if        %found
     c     rsky          chain     prs01

     c                   if        %found
     c     rrky          setgt     prr01
     c     rrky1         readpe    prr01

     c                   if        (not %eof)

      * Return Requested Name / Address / FEIN information

     c                   select

     * Not PEO

     c                   when      fnpeof = *blank
     c                   eval      qqname = CLT_name
     c                   eval      qqfnam = CLT_fnam
     c                   eval      qqead1 = CLT_ead1
     c                   eval      qqead2 = CLT_ead2
     c                   eval      qqcty  = CLT_cty
     c                   eval      qqst   = CLT_st
     c                   eval      qqzip  = CLT_zip
     c                   eval      qqfein = CLT_fein

     * PEO output control = 1

     c                   when      rrpgfl = '1'
     c                   eval      qqname = PEO_name
     c                   eval      qqfnam = PEO_fnam
     c                   eval      qqead1 = PEO_ead1
     c                   eval      qqead2 = PEO_ead2
     c                   eval      qqcty  = PEO_cty
     c                   eval      qqst   = PEO_st
     c                   eval      qqzip  = PEO_zip
     c                   eval      qqfein = PEO_fein

     * PEO output control = 2

     c                   when      rrpgfl = '2'
     c                   eval      qqname = CLT_name
     c                   eval      qqfnam = CLT_fnam
     c                   eval      qqead1 = CLT_ead1
     c                   eval      qqead2 = CLT_ead2
     c                   eval      qqcty  = CLT_cty
     c                   eval      qqst   = CLT_st
     c                   eval      qqzip  = CLT_zip
     c                   eval      qqfein = CLT_fein

     * PEO output control = 3 or blank

     c                   when      (rrpgfl = '3') or (rrpgfl = ' ')
     c*** begin chg ***  03/22/11  ******************************************************
     c*********          eval      ##line  = %trim(PEO_name)
     c*********                              + ' L/C/F '
     c*********                              + %trim(CLT_name)
     C*** begin chg ***  02/24/17  ***************************************
     c******             eval      ##line  = %trim(PEO_name)
     c******                                 + ' L/C/F '
     c******                                 + %trim(CLT_fnam)
     c******                                 + ' DBA '
     c******                                 + %trim(CLT_name)
     c                   Clear                   flexPEOA         10
     C                   If        RRPEOA <> ' '
     C                   Eval      flexPEOA = RRPEOA
     C                   Else
     C                   Eval      flexPEOA = 'L/C/F'
     C                   Endif
     c
     C*** begin add ***  04/05/24  **************************************
     c                   if        PEO_fnam <> ' '
     c                   eval      PEO_name = PEO_fnam
     c                   endif
     C*** end   add ***  04/05/24  **************************************
     C*** begin add ***  07/14/25  **************************************
     C                   Select
     C                   When      cltID = '148'
     c                   If        CLT_fnam <> ' '
     c                   Eval      ##line  = %trim(PEO_name) + ' '
     c                                       + %trim(flexPEOA) + ' '
     c                                       + %trim(CLT_fnam)
     c                                       + ' '
     c                                       + %trim(CLT_name)
     C                   Else
     c                   Eval      ##line  = %trim(PEO_name) + ' '
     c                                       + %trim(flexPEOA) + ' '
     c                                       + %trim(CLT_name)
     C                   Endif
     C                   Other
     C*** end add *****  07/14/25  **************************************
     c                   If        CLT_fnam <> ' '
     c                   Eval      ##line  = %trim(PEO_name) + ' '
     c                                       + %trim(flexPEOA) + ' '
     c                                       + %trim(CLT_fnam)
     c                                       + ' DBA '
     c                                       + %trim(CLT_name)
     C                   Else
     c                   Eval      ##line  = %trim(PEO_name) + ' '
     c                                       + %trim(flexPEOA) + ' '
     c                                       + %trim(CLT_name)
     C                   Endif
     C*** begin add ***  07/14/25  **************************************
     C                   Endsl
     C*** end add *****  07/14/25  **************************************
     C*** end chg *****  02/24/17  ***************************************
     c*** end chg *****  03/22/11  ******************************************************
     c                   eval      ##length = 100
     c                   call      'SRLINE'      linepl
     c                   eval      qqname  = %trim(##line)
     c                   eval      qqfnam  = %trim(##remain)
     c                   eval      qqead1 = CLT_ead1
     c                   eval      qqead2 = CLT_ead2
     c                   eval      qqcty  = CLT_cty
     c                   eval      qqst   = CLT_st
     c                   eval      qqzip  = CLT_zip
     c                   eval      qqfein = CLT_fein
     C*** begin add ***  03/28/17  ************************************************
     c                   If        rrpgfl = '3'
     c                   Eval      qqfein = PEO_fein
     c                   Endif
     C*** end add *****  03/28/17  ************************************************

     c                   endsl

      * assemble city/state/zip

     c                   eval      qqecsz = %trim(qqcty)
     c                                      + ', '
     c                                      + qqst
     c                   movel     qqzip         xxzip5            5
     c                   move      qqzip         xxzip4            4

     c                   if        xxzip5 <> *zero
     c                   eval      qqecsz = %trim(qqecsz) + ' ' + xxzip5
     c                   endif

     c                   if        xxzip4 <> *zero
     c                   eval      qqecsz = %trim(qqecsz) + '-' + xxzip4
     c                   endif

     c                   eval      qqeadr = %trim(qqead1) + ','
     c                             + %trim(qqecsz)
     c                   endif
    
     c                   endif
    
     c                   endif
    
     c                   endif

     c                   eval      *inlr = *on
    
    
     *=========================================================================
     * getPEO - Get PEO address information
     *=========================================================================
     c     getPEO        begsr

     c                   eval      xxemp# = 1
     c                   eval      xxdiv  = 0
     c     emky1         chain     pem01

     c                   if        %found
     c                   eval      PEO_name = emname + emnam2
     c                   eval      PEO_fnam = emfdnm + emfdn2

     c                   if        (emmpfl = 'P')
      * Physical Address
     c                   eval      PEO_ead1 = empad1
     c                   eval      PEO_ead2 = empad2
     c                   eval      PEO_cty  = empcty
     c                   eval      PEO_st   = empst
     c                   eval      PEO_zip  = empzip

     c                   else
      * Mailing Address
     c                   eval      PEO_ead1 = emmad1
     c                   eval      PEO_ead2 = emmad2
     c                   eval      PEO_cty  = emmcty
     c                   eval      PEO_st   = emmst
     c                   eval      PEO_zip  = emmzip
     c                   endif

     c                   eval      PEO_fein = emfein
     C*** begin add ***  01/15/18  **************************************
     C*
     C* Override PEO name with Additional Named Insured from PEO.
     C*
     C                   If        xxSeq# <> 0
     C     ANKY          CHAIN     PAN01                              60
     C                   If        *in60 = *off
     c                   Eval      PEO_name = ANname + ANnam2
     c                   eval      PEO_fein = ANfein
     C                   Clear                   PEO_fnam
     C                   Endif
     C                   Endif
     C*** end add *****  01/15/18  **************************************
     c                   endif

     c                   endsr
    
    
     *=========================================================================
     * getCLT - Get Client address information
     *=========================================================================
     c     getCLT        begsr

     c     emky          chain     pem01

     c                   if        %found
     c                   eval      CLT_name = emname + emnam2
     c                   eval      CLT_fnam = emfdnm + emfdn2

     c                   if        (emmpfl = 'P')
      * Physical Address
     c                   eval      CLT_ead1 = empad1
     c                   eval      CLT_ead2 = empad2
     c                   eval      CLT_cty  = empcty
     c                   eval      CLT_st   = empst
     c                   eval      CLT_zip  = empzip

     c                   else
      * Mailing Address
     c                   eval      CLT_ead1 = emmad1
     c                   eval      CLT_ead2 = emmad2
     c                   eval      CLT_cty  = emmcty
     c                   eval      CLT_st   = emmst
     c                   eval      CLT_zip  = emmzip
     c                   endif

     c                   eval      CLT_fein = emfein
     c                   endif

     c                   endsr
    
    

