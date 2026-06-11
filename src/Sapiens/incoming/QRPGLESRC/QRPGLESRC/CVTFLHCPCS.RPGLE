     H* (C) Copyright Progressive Data Solutions, 2004
     H/TITLE CVTFLHCPCS                                                                     *
     H**************************************************************************************
     H*  Description:  Add HCPCS Codes                                                     *
     H*                                                                                    *
     H*  Created: 03/12/04   Programmer C:79    Support:41483                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FFMPRLFL   UF A E           K DISK
     FFLHCPCS   IF   E             DISK    usropn
     FSTSTL     IF   E           K DISK
     FSMCOL     IF   E           K DISK

     D ovrdbf          c                   const('OVRDBF FILE(FLHCPCS) TOFILE+
     D                                     (PDSDATA/FLHCPCS)')
     D dltovr          c                   const('DLTOVR FLHCPCS')
     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D kkefdt          s                   like(prefdt) inz(1040101)
     D kkco#           s                   like(prco#)
     D kkcpt4          s                   like(prcpt4)
     D kkkey           s                   like(stkey) inz('FL')

     DPSDS           ESDS                  Extname(SZQ1P)
    C*================================================================
     C/COPY SPRRUNCMD
    C*================================================================
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkefdt
     C*
     C     stky          klist
     C                   kfld                    kkkey
     C*
     C*----------------------------------------------------------------------
     C*  Is Florida an installed state for this client
     C*----------------------------------------------------------------------
     C*
     C     stky          chain     pst01
     C                   if        %found
     C*
     C*----------------------------------------------------------------------
     C*  Override and open the file
     C*----------------------------------------------------------------------
     C                   callp     runCmd(ovrdbf : 44)
     C                   open      FLHCPCS
     C*
     C*----------------------------------------------------------------------
     C*  Read through flat file
     C*----------------------------------------------------------------------
     C*
     C                   read      pco01
     C                   dow       not %eof(smcol)
     C*
     C                   eval      kkco# = coco#
     C*
     C     1             setll     flhcpcs
     C                   read      flhcpcs
     C                   dow       not %eof(flhcpcs)
     C*
     C                   eval      kkcpt4 = prcpt4
     C     prky          chain     ppr01
     C                   if        not %found
     C                   exsr      write_fmprpfl
     C                   endif
     C*
     C                   read      hcpcs
     C                   enddo                                                  *in60=*off
     C*
     C                   read      pco01
     C                   enddo
     C*
     C                   close     FLHCPCS
     C                   callp     runCmd(dltovr : 14)
     C                   endif                                                  If not *in99
     C*
     C                   move      *on           *inlr
     C*=====================================================================
     C* Write_FMPRPFL - Write to FMPRPFL
     C*=====================================================================
     C     write_fmprpfl begsr
     C*
     C                   eval      prco# = kkco#

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
