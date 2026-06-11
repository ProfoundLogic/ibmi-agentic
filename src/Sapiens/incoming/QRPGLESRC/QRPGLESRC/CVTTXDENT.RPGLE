     H* (C) Copyright Progressive Data Solutions, 2006
     H/TITLE CVTTXDENT                                                                     *
     H**************************************************************************************
     H*  Description:  Convert from flat file to FMPRPTX                                   *
     H*                                                                                    *
     H*  Created: 02/14/06   Programmer C:163   Project:TX034                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H*  06/12/08  C: 180  PROJ  TX042 - Accept co# AND EFF DATE AS PARMS
     H*
     H*  06/02/08  C: 180  Proj: TX040 - MAR 1 2008 update                                 *
     H*
     H*  05/23/08  C: 180  Proj: TX042 - Jan 1 2008 update                                 *
     H*
     H*  02/20/07  C: 079  Proj: TX034                                                     *
     H*                    Updated for 2007 Fee Schedule                                   *
     H*                                                                                    *
     H**************************************************************************************
     FFMPRLTX   UF A E           K DISK
     FTXDENT    IF   E             DISK
      ***** BEGIN ADD ******* 05/23/08 *************************************
     FFMPRLUC   IF   E           K DISK    RENAME(PPR01:PRLUC) PREFIX(U_)
      ***** END ADD ********* 05/23/08 *************************************

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
      ******** BEGIN CHANGE ***** 06/02/08 *********************************
     D***** kkefdt          s                   like(prefdt) inz(1080101)
     D kkefdt          s                   like(prefdt) inz
      ******** END CHANGE ******* 06/02/08 *********************************
     D kkco#           s                   like(prco#)  inz
     D kkcpt4          s                   like(prcpt4)

     DPSDS           ESDS                  Extname(SZQ1P)

     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkefdt
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C     *ENTRY        PLIST
     C                   PARM                    INCO#             3
     C                   PARM                    INEFDT            7
      *
     C                   read      ptx01                                  99
     C                   dow       *in99  = *off
     C*
     C                   eval      kkcpt4 = txcode
     C     prky          chain     ppr01                              99
     C                   if        *in99  = *on
     C                   exsr      write_fmprptx
     C                   else
     C                   exsr      updat_fmprptx
     C                   endif
     C*
     C                   read      ptx01                                  99
     C                   enddo                                                  *in60=*off
     C*
     C                   move      *on           *inlr
     C*=====================================================================
     C* Move_data_w - Moves write specific data
     C*=====================================================================
     C     move_data_w   begsr
     C*
     C                   eval      PRCO#  = kkco#
     C                   eval      PREFDT = kkefdt
     C                   eval      PRCPT4 = TXCODE
     C                   eval      PRDSC1 = *Blanks
     C                   eval      PRDSC2 = *Blanks
     C*
      ******** BEGIN CHANGE ******* 05/23/08 *************************
     C****               if        TXRPT  = ' '
     C****               eval      PRRPT  = 'N'
     C****               else
     C****               eval      PRRPT  = TXRPT
     C****               endif
      *
     C                   if        TXMRA = *ZERO
     C                   eval      PRRPT  = 'Y'
     C                   else
     C                   eval      PRRPT  = 'N'
     C                   endif
      ******** END CHANGE ********* 05/23/08 *************************
     C*
     C                   eval(H)   PRMRA  = 2 * TXMRA
     C                   eval(H)   PR92N6 = PRMRA
     C                   eval(H)   PR92N8 = PRMRA
     C                   eval(H)   PR92N9 = PRMRA
     C                   eval(H)   PR92NE = PRMRA
     C                   eval(H)   PR92NF = PRMRA
     C                   eval(H)   PR92NK = PRMRA
     C                   eval(H)   PR92NL = PRMRA
     C                   eval(H)   PR92NQ = PRMRA
     C                   eval(H)   PR92NR = PRMRA
     C                   eval(H)   PR92NW = PRMRA
     C                   eval(H)   PR92NX = PRMRA
     C                   eval(H)   PR92O3 = PRMRA
     C                   eval(H)   PR92O4 = PRMRA
     C                   eval(H)   PR92O9 = PRMRA
     C                   eval(H)   PR92OA = PRMRA
     C                   eval      PR1A7  = 'M'
     C*                  eval      PR1A8  = '9'
     C*                  eval      PR1A9  = '9'
     C*                  eval      PR1AA  = '9'
     C*                  eval      PR1AB  = '9'
     C*                  eval      PR1AC  = '9'
     C*                  eval      PR1AD  = '9'
     C                   eval      PRFSR  = 'Y'
     C                   eval      PRSTAT = 'A'
     C                   eval      PRSURG = 'N'
     C                   eval      PRSUPP = 'N'
     C*
      ******* BEGIN ADD ******* 05/23/08 ******************************
     C     prky          chain     FMPRLUC
     C                   if        %found(FMPRLUC)
     C                   movel     U_PRDSC1      PRDSC1
     C                   movel     U_PRDSC2      PRDSC2
     C                   endif
      ******* END ADD   ******* 05/23/08 ******************************
     C                   endsr
     C*=====================================================================
     C* Write_fmprptx - Writes FMPRPTX
     C*=====================================================================
     C     write_fmprptx begsr
     C*
     C                   clear                   ppr01
     C*
     C                   exsr      move_data_w
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      prdt
     C     *cymd         move      sys_date      pridt
     C                   eval      prpgm=q1pgm
     C                   eval      pruser=q1user
     C                   eval      priusr=q1user
     C                   move      sys_time      prtime
     C*
     C                   write     ppr01
     C*
     C                   endsr
     C*=====================================================================
     C* Update_fmprptx - Updates FMPRPTX
     C*=====================================================================
     C     updat_fmprptx begsr
     C*
     C******* BEGIN CHANGE ****** 05/23/08 ***************************
     C*****              if        TXRPT  = ' '
     C*****              eval      PRRPT  = 'N'
     C*****              else
     C*****              eval      PRRPT  = TXRPT
     C*****              endif
      *
     C                   if        TXMRA = *ZERO
     C                   eval      PRRPT  = 'Y'
     C                   else
     C                   eval      PRRPT  = 'N'
     C                   endif
     C******* END CHANGE ******** 05/23/08 ***************************
     C*
     C                   eval(H)   PRMRA  = TXMRA * 2
     C                   eval(H)   PR92N6 = PRMRA
     C                   eval(H)   PR92N8 = PRMRA
     C                   eval(H)   PR92N9 = PRMRA
     C                   eval(H)   PR92NE = PRMRA
     C                   eval(H)   PR92NF = PRMRA
     C                   eval(H)   PR92NK = PRMRA
     C                   eval(H)   PR92NL = PRMRA
     C                   eval(H)   PR92NQ = PRMRA
     C                   eval(H)   PR92NR = PRMRA
     C                   eval(H)   PR92NW = PRMRA
     C                   eval(H)   PR92NX = PRMRA
     C                   eval(H)   PR92O3 = PRMRA
     C                   eval(H)   PR92O4 = PRMRA
     C                   eval(H)   PR92O9 = PRMRA
     C                   eval(H)   PR92OA = PRMRA
     C                   eval      PR1A7  = 'M'
     C*                  eval      PR1A8  = '9'
     C*                  eval      PR1A9  = '9'
     C*                  eval      PR1AA  = '9'
     C*                  eval      PR1AB  = '9'
     C*                  eval      PR1AC  = '9'
     C*                  eval      PR1AD  = '9'
     C                   eval      PRFSR  = 'Y'
     C                   eval      PRSTAT = 'A'
     C                   eval      PRSURG = 'N'
     C                   eval      PRSUPP = 'N'
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      prdt
     C                   eval      prpgm=q1pgm
     C                   eval      pruser=q1user
     C                   move      sys_time      prtime
     C*
     C                   update    ppr01
     C*
     C                   endsr
     C*=====================================================================
     C* Initialization
     C*=====================================================================
     C     *INZSR        begsr
     C                   if        INCO# > *BLANK
     C                   MOVE      INCO#         kkco#
     C                   ELSE
     C                   MOVE      *ON           *INLR
     C                   ENDIF
     C                   if        INEFDT > *BLANK
     C                   MOVE      INEFDT        kkefdt
     C                   ELSE
     C                   MOVE      *ON           *INLR
     C                   ENDIF
     C                   endsr
