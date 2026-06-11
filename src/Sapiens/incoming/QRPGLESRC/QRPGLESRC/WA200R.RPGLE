     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio: *srcstmt)                                                                *
     H debug(*yes)                                                                                 *
     H dftactgrp(*no) actgrp(*caller)
      *
     h/title WA200R - Auto Assign Carrier #
      *=====================================================================*
      *                                                                     *
      * Program description:                                                *
      *   This program assigns a carrier policy number for an employer      *
      *   based on the criteria assigned for the group.                     *
      *                                                                     *
      * CALLED FROM:                                                        *
      *   - W8700R   - Transfer QUOTE to LIVE                               *
      *   - W7820R   - Renewal                                              *
      *   - W0051R02 - Policy Year Add                                      *
      *   - W8393R   - PEO import                                           *
      *                                                                     *
      * CALLS TO:                                                           *
      *     none                                                            *
      *                                                                     *
      *=====================================================================*
      * REVISIONS:                                                          *
      *
      * 03/22/17 C:33  Project: 148WAB                                      *
      *                Do not use 'M' for CLT148.                           *
      *
      * 01/12/15 C:180 Support 32152                                        *
      *                Do not increase carrier number for same employer.    *
      *                                                                     *
      * 04/01/13 C:282 Project: 147WAK
      *                Recompile Only.
      *
      * 03/02/09 C:61  Support WA135 67431
      *                Move "M - Effective month" insertion prior
      *                "& - State" insertion.
      *                                                                     *
      * 08/30/07 C:65  Supp#59904                                           *
      *                Pull the carrier # from the state reporting div      *
      *                for any additional divisions.  If there is no record *
      *                for state reporting division then prevent the write  *
      *                of a record for additional division.                 *
      *                                                                     *
      * 07/10/07 C:65  Supp#58317                                           *
      *                Pull the state for carrier # from state rpt div.     *
      *                                                                     *
      * 12/06/06 C:61  WC1444                                               *
      *                Copied from W8700R4 and modified.                    *
      *                                                                     *
      *=====================================================================*

     Hdatedit(*ymd) datfmt(*ymd)

      * Group Carrier Information
     FWMGCL     uf   e           k disk

      * Employer Policy Year
     FWDELL     if   e           k disk

      * Rating State
     FWMRSL     if   e           k disk                                         User Parameters

      * Carrier State/Reporting Master
     Fwmrpl     if a e           k disk
     Fwmrpl4    if   e           k disk    rename(prp01:prp00) prefix(d0_)
     f*** begin add ***  07/10/07  *************************************************
     Fwmeml     if   e           k disk
     f*** end add *****  07/10/07  *************************************************
     f*** begin add ***  01/12/15  *************************************************
     FWMRPL6    if   e           k disk    rename(prp01:prp02) prefix(d2_)
     f*** end add *****  01/12/15  *************************************************


      *=========================================================================
      *  Data Structure definitions
      *=========================================================================
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)


      *=========================================================================
      *  Field definitions
      *=========================================================================
    
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
    
     d  cpol           s              1a   dim(18)
     d  xxcpol         s             18a
     d  xxchar         s              1a
     d  xxtext         s             18a

     d  begin#         s              3  0
     d  end#           s              3  0
     d  length#        s              3  0
     d  start#         s              3  0
     d  inserted       s              1a
     d  max_length     s              3  0
     d  x1             s              3  0

     d  xxdiv0         s              5  0 inz(0)
     d  xxpfdt         s                   like(elpfdt)

     d  psds         esds                  extname(szq1p)
     f*** begin add ***  07/10/07  *************************************************
     d  xsdiv          s                   like(emdiv)
     f*** end add *****  07/10/07  *************************************************
     d*** begin add ***  01/12/15  *************************************************
     d  xxacn          s                   like(rpacn)
     d*** end add *****  01/12/15  *************************************************
     D***  Begin Add  ***  03/22/17  **************************************
     DskipM            s              1n   inz(*off)
     D***  End   Add  ***  03/22/17  **************************************
    
    
      *=========================================================================
      *  KLIST definitions
      *=========================================================================

      *  Group Carrier Information

     c     gcky          klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
    
      *  Employer Policy Year
    
     c     elky          klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@emp#
     c                   kfld                    @@div
     c                   kfld                    @@fyr

     c     elky1         klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@emp#
     c                   kfld                    @@div
     c*** begin add ***  07/10/07  *************************************************
    
     c     elky2         klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@emp#
     c                   kfld                    xsdiv
     c                   kfld                    @@fyr

      *  Employer Master file
    
     c     emky          klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@emp#
     c*** end add *****  07/10/07  *************************************************

      *  Rate Set Code

     c     rsky          klist
     c                   kfld                    elco#
     c                   kfld                    elrtcd

      *  Carrier State/Reporting

     c     rpky2         klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
     c                   kfld                    xxdiv0
     c*** begin add ***  08/30/07  **************************************
     c     rpky3         klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@emp#
     c                   kfld                    xsdiv
     c*** end add *****  08/30/07  **************************************

     c*** begin add ***  01/12/15  **************************************
     c     rpky4         klist
     c                   kfld                    @@co#
     c                   kfld                    @@fnd
     c                   kfld                    @@emp#
     c*** end add *****  01/12/15  **************************************

      *=========================================================================
      *  PLIST definitions
      *=========================================================================

      *  Entry parameters

     c     *entry        plist
     c                   parm                    @@co#             3 0
     c                   parm                    @@fnd             3 0
     c                   parm                    @@emp#            9 0
     c                   parm                    @@div             5 0
     c                   parm                    @@fyr             3 0
     c                   parm                    @@pgm            10
    
    
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr


      *=========================================================================
      *  Main
      *=========================================================================
    
     * get client Id
    
     c                   call      'RTVCLTID'
     c                   parm                    client#           3

     C***  Begin Add  ***  03/22/17  **************************************
     C                   select
     C                   when      client# = '148'
     C                   eval      skipM = *on
     C                   endsl
     C***  End   Add  ***  03/22/17  **************************************
    
     * get group carrier info
    
     c     gcky          CHAIN(N)  pgc01

     c                   if        %found and (gccfmt <> *blank)

     c                   select

      *  Quote Transfer
     c                   when      (@@pgm = 'W8700R')
     c                             and (gcacfl = 'Y')
     c                   exsr      Create_new#

      *  Renewal
     c                   when      (@@pgm = 'W7820R')
     c                             and (gcucfl = 'Y')
     c                   exsr      Create_new#

      *  Policy Year Add
     c                   when      (@@pgm = 'W0051R02')
     c                   exsr      Create_new#

      *  PEO Import
     c                   when      (@@pgm = 'W8393R')
     c                   exsr      Create_new#

     c                   endsl

     c                   endif

     c                   eval      *inlr = *on
    

      *=====================================================================
      * Create_new# - Create new Carrier # for policy using group template
      *=====================================================================
     c     Create_new#   begsr
     c                   move      gccfmt        xxcpol
     c*** begin add ***  07/10/07  *************************************************
     c     emky          chain     pem01                              60
     c     *in60         doweq     *off
    
     c                   if        emstrp = 'Y'
     c                   move      emdiv         xsdiv
     c                   leave
     c                   endif
    
     c     emky          reade     pem01                                  60
     c                   enddo
     c*** end add *****  07/10/07  *************************************************
    
     * get employer policy year info
    
     c     elky          CHAIN     pel01

     c                   if        %found
     c                   eval      xxpfdt = elpfdt
    
     *  (#) - next available carrier number
    
     c                   eval      max_length = 9
     c                   eval      xxchar = '#'
     c                   clear                   xxtext
     c*** begin add ***  01/12/15  *************************************************
      * Check if carrier number already assigned
      *
     c     rpky4         Chain     prp02                              77
     c                   select
     c                   when      *in77 = *off
     c     rpky4         setgt     prp02
     c                   if        not %found
     c     *hival        setgt     prp02
     c                   endif
     c     rpky4         readpe    prp02
     c                   if        not %eof  and d2_rpacn > *zero
     c                   move      d2_rpacn      xxtext
     c                   move      d2_rpacn      xxacn
     c                   else
     c                   move      gcnacn        xxtext
     c                   move      gcnacn        xxacn
     c                   eval      *in77 = *on
     c                   endif
     c                   other
     c                   move      gcnacn        xxacn
     c*** end add ***  01/12/15  *************************************************
     c                   move      gcnacn        xxtext
     c                   endsl
     c                   exsr      insert_text

     c                   if        inserted = 'Y'
     c*** begin add ***  01/12/15  *************************************************
     c                   If        *in77
     c*** End   add ***  01/12/15  *************************************************

     c     gcky          CHAIN     pgc01

     c                   if        %found
     c                   eval      gcnacn = gcnacn + 1
     c                   eval      gcpgm  = q1pgm
     c                   eval      gcuser = q1user

     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         gcdt

     c                   eval      gctime = dstime

     c                   update    pgc01
     c                   endif
     c*** begin add ***  01/12/15  *************************************************
     c                   endif
     c*** End   add ***  01/12/15  *************************************************
    
     c                   endif
    
     *  ($) - Employer number
    
     c                   eval      max_length = 9
     c                   eval      xxchar = '$'
     c                   clear                   xxtext
     c                   move      @@emp#        xxtext
     c                   exsr      insert_text
    
     *  (+) - Group number
    
     c                   eval      max_length = 3
     c                   eval      xxchar = '+'
     c                   clear                   xxtext
     c                   move      @@fnd         xxtext
     c                   exsr      insert_text
    
     *  (@) - Policy Year (001 002 003 004 ...)
    
     c                   eval      x1 = 0
     c     elky1         SETLL     pel01
     c     elky1         READE     pel01
    
     c                   dow       not %eof
     c                   add       1             x1

     c                   if        elfyr = @@fyr
     c                   leave
     c                   endif

     c     elky1         reade     pel01
     c                   enddo
    
     c                   eval      max_length = 3
     c                   eval      xxchar = '@'
     c                   clear                   xxtext
     c                   move      x1            xxtext
     c                   exsr      insert_text
    
     *  (%) - Policy Year (099 100 101 102 ...)
    
     c                   eval      max_length = 3
     c                   eval      xxchar = '%'
     c                   clear                   xxtext
     c                   move      @@fyr         xxtext
     c                   exsr      insert_text
      ******* beg add ****** 03/02/09 *********************************

     *  (M) - Effective Month

     c     elky          CHAIN     pel01

     c                   if        %found
     C***  Begin Add  ***  03/22/17  **************************************
     C                              and not skipM
     C***  End   Add  ***  03/22/17  **************************************
     c                   move      elpfdt        xxmmdd            4
     c                   movel     xxmmdd        xxmm              2
     c                   eval      max_length = 2
     c                   eval      xxchar = 'M'
     c                   clear                   xxtext
     c                   move      xxmm          xxtext
     c                   exsr      insert_text
     c                   endif
    
      ******* end add ****** 03/02/09 *********************************
    
     *  (&) - State code
    
     c*** begin add ***  07/10/07  *************************************************
     c*****elky****      CHAIN     pel01
     c     elky2         CHAIN     pel01
     c*** end add *****  07/10/07  *************************************************

     c                   if        %found
     c     rsky          CHAIN     prs01

     c                   if        %found
     c                   eval      max_length = 2
     c                   eval      xxchar = '&'
     c                   clear                   xxtext
     c                   move      rsjrst        xxtext
     c                   exsr      insert_text
     c                   endif
    
     c                   endif
    
      ******* beg del ****** 03/02/09 *********************************
     ***(M)*-*Effective Month
      *********
     c*****elky          CHAIN     pel01
      *********
     c*********          if        %found
     c*********          move      elpfdt        xxmmdd            4
     c*********          movel     xxmmdd        xxmm              2
     c*********          eval      max_length = 2
     c*********          eval      xxchar = 'M'
     c*********          clear                   xxtext
     c*********          move      xxmm          xxtext
     c*********          exsr      insert_text
     c*********          endif
     *********
      ******* end del ****** 03/02/09 *********************************
      ****
    
     *  Write new Carrier # (WMRPP) record
    
     c                   exsr      createcpol
     c                   endif

     c                   endsr


      *=====================================================================
      * Insert_text - Insert replacement text (right justified) into string
      *=====================================================================
     c     Insert_text   begsr
    
     c                   eval      inserted = 'N'
     c                   movea     xxcpol        cpol
     c     xxchar        SCAN      xxcpol        begin#

     c                   if        begin# <> 0
     c                   eval      end# = begin#

     c                   dow       (end# < 19) and (cpol(end#) = xxchar)
     c                   eval      end# = end# + 1
     c                   enddo

     c                   eval      length# = end# - begin#

     c                   if        length# > max_length
     c                   eval      length# = max_length
     c                   endif

     c                   eval      start# = 19 - length#

     c                   eval      %subst(xxcpol:begin#:length#) =
     c                             %subst(xxtext:start#:length#)
    
     c                   eval      inserted = 'Y'
     c                   endif

     c                   endsr


      *=====================================================================
      * createcpol - subroutine to create carrier policy number
      *=====================================================================
     c     createcpol    BEGSR

     c                   clear                   prp01
     c                   eval      rpco# = @@co#
     c                   eval      rpfnd = @@fnd
     c                   eval      rpemp# = @@emp#
     c                   eval      rpdiv = @@div
     c                   eval      rpfyr = @@fyr

     c                   eval      rpcarr = gcdcar
     c                   movel(p)  xxcpol        rpcap#
     c*** Begin Add **** 01/12/15 ***************************************
     c                   move      xxacn         rpacn
     c*** End   Add **** 01/12/15 ***************************************

     c                   eval      rpiusr = q1user
     c                   eval      rpuser = q1user
     c                   eval      rpipgm = q1pgm
     c                   eval      rppgm = q1pgm

     c                   time                    dsdttm
     c                   Z-ADD     dsdate        srdti
     c                   CALL      'SRCVTDAT'    dtcymd
     c                   movel     srdto         rpidt
     c                   movel     srdto         rpdt

     c                   eval      rpitm = dstime
     c                   eval      rptime = dstime
     c*** begin add ***  08/30/07  **************************************
     c                   move      *blank        xxwtr             1
     c
     c                   If        @@div <> xsdiv
     c     rpky3         Chain     prp00

     c                   If        %found
     c                   move      'Y'           xxwtr
     c                   eval      rpcap# = d0_rpcap#
     c*** Begin Add **** 01/12/15 ***************************************
     c                   move      d0_rpacn      rpacn
     c*** End   Add **** 01/12/15 ***************************************
     c                   Endif

     c                   else
     c                   move      'Y'           xxwtr
     c                   Endif
     c*** end add *****  08/30/07  **************************************

     c                   If        client# = '135' and xxpfdt < 1050601
     c                               and @@div <> 0
     c     rpky2         Chain     prp00

     c                   If        %found
     c                   eval      rpcap# = d0_rpcap#
     c                   Endif

     c                   Endif

     c*** begin add ***  08/30/07  **************************************
     c                   if        xxwtr = 'Y'
     c*** end add *****  08/30/07  **************************************
     c                   write     prp01
     c*** begin add ***  08/30/07  **************************************
     c                   endif
     c*** end add *****  08/30/07  **************************************

     c                   ENDSR


