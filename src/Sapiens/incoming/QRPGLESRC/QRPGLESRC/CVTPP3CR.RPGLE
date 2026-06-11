     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      /title CVTPP3CR - Load 3C Info to Group Print File
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
      *===============================================================
      * CALLED FROM: CVTPP3CL1                                       *
      *                                                              *
      * Note: This program will update the ELFFLA (overwrite) flag   *
      *       and ELPP3C/ELPPC3 (3C Info) fields depending           *
      *                                                              *
      *                                                              *
      * Created 12/31/10  C:255  Project: WC1918                     *
      *===============================================================
      * Revision:                                                    *
      *                                                              *
      * 08/23/11 C:136 Project: WC1918                               *
      *                Rewrite program (Changes not commented)       *
      *===============================================================
     fwdf4l     if a e           k disk
     fwdf2l     if   e           k disk
     fwmfnl     if   e           k disk
     fwdell4    uf   e           k disk
     fwmeml     if   e           k disk
     fsmorl     if a e           k disk

     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d sysdt           s              7  0
     d kkco#           s                   like(F4CO#)
     d kkfnd           s                   like(F4FND)
     d kkkey           s                   like(F4KEY) inz('PP3C')
     d cxDate          s                   like(EMCAD1)
     d kkpnam          s                   like(ORPNAM) inz('QRYPP3CR')
     d kkfil#          s                   like(ORFIL#) inz('0')

     d                 ds
     d dsdttm                        12  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        6  0 overlay(dsdttm:7)

     dpsds           esds                  extname(SZQ1P)

     c     f2ky          klist
     c                   kfld                    FNCO#
     c                   kfld                    FNFND

     c     f4ky          klist
     c                   kfld                    FNCO#
     c                   kfld                    FNFND
     c                   kfld                    kkkey
     c                   kfld                    FNEFDT

     c     emky          klist
     c                   kfld                    ELCO#
     c                   kfld                    ELFND
     c                   kfld                    ELEMP#
     c                   kfld                    ELDIV

     c     orky          klist
     c                   kfld                    kkpnam
     c                   kfld                    kkfil#

     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
    
     c                   time                    dsdttm
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         sysdt
    
     c                   call      'RTVCLTID'
     c                   parm                    cltid             3
      * exit if client has modified code that inhibits WC1918
     c                   if        cltid = '065' or
     c                             cltid = '093' or
     c                             cltid = '048' or
     c                             cltid = '081' or
     c                             cltid = '091' or
     c                             cltid = '129'
     c                   seton                                        lr
     c                   return
     c                   endif

     c                   read      pfn01
     c                   dow       not(%eof)
     c                   clear                   pf201
     c     f2ky          chain     pf201
     c                   if        %found(WDF2L)
     c     f4ky          chain     pf401
     c                   if        not(%found(WDF4L))
     c                   exsr      wrtWDF4P
     c                   endif
     c                   endif                                                  not(%found(WDF2L))
     c                   exsr      updateWDELP
     c                   read      pfn01
     c                   enddo
     c                   exsr      wrtSMORP
     c                   seton                                        lr
      *===============================================================
      * wrtWDF4P - write 3C info to group print file
      *===============================================================
     c     wrtWDF4P      begsr
     c                   eval      F4STAT = 'A'
     c                   eval      F4CO#  = F2CO#
     c                   eval      F4FND  = F2FND
     c                   eval      F4KEY  = 'PP3C'
     c                   eval      F4DESC = 'Policy Page 3c'
     c                   eval      F4TXT1 = F2PP3C
     c                   eval      F4TXT2 = F2PPC3
     c                   eval      F4EFDT = FNEFDT
     c                   eval      F4IUSR = Q1USER
     c                   eval      F4IPGM = Q1PGM
     c                   eval      F4USER = Q1USER
     c                   eval      F4PGM  = Q1PGM
     c                   eval      F4IDT  = sysDt
     c                   eval      F4ITIM = dstime
     c                   eval      F4DT   = sysDt
     c                   eval      F4TIME = dstime
     c                   write     pf401
     c                   endsr
      *===============================================================
      * updateWDELP - update 'Overwrite Group 3c' flag or
      *               clear 3c fields
      *===============================================================
     c     updateWDELP   begsr

     c     f4ky1         klist
     c                   kfld                    FNCO#
     c                   kfld                    FNFND
     c                   kfld                    kkkey

     c     f4ky2         klist
     c                   kfld                    FNCO#
     c                   kfld                    FNFND
     c                   kfld                    kkkey
     c                   kfld                    sysdt

     c     f2ky          setll     pel01
     c     f2ky          reade     pel01
     c                   dow       not(%eof)
     c     emky          chain     pem01
     c                   if        %found
     c                   clear                   pf401
     c     f4ky2         setgt     pf401
     c     f4ky1         readpe    pf401
     c                   select
     c                   when      EMEFD5 <> *zero
     c                   eval      cxDate  = EMCAD5
     c                   when      EMEFD4 <> *zero
     c                   eval      cxDate  = EMCAD4
     c                   when      EMEFD3 <> *zero
     c                   eval      cxDate  = EMCAD3
     c                   when      EMEFD2 <> *zero
     c                   eval      cxDate  = EMCAD2
     c                   when      EMEFD1 <> *zero
     c                   eval      cxDate  = EMCAD1
     c                   endsl
     c                   select
     c                   when      ELFFLA = 'Y'
     c                   when      ELPTDT <= sysDt  or
     c                             cxDate <= sysDt  and cxDate > 0
     c                   eval      ELFFLA = 'Y'
     c                   when      ELPP3C =  F4TXT1 and
     c                             ELPPC3 =  F4TXT2
     c                   clear                   ELPP3C
     c                   clear                   ELPPC3
     c                   when      ELPP3C <> F4TXT1  or
     c                             ELPPC3 <> F4TXT2
     c                   eval      ELFFLA = 'Y'
     c                   endsl
     c                   update    pel01
     c                   endif                                                  %found(WMEMP)
     c     f2ky          reade     pel01
     c                   enddo
     c                   endsr
      *===============================================================
      * wrtSMORP - write routing for qry report
      *===============================================================
     c     wrtSMORP      begsr
     c     orky          chain     por01
     c                   if        not(%found)
     c                   eval      ORPNAM = 'QRYPP3CR'
     c                   eval      ORFIL# = '0'
     c                   eval      OROUTQ = 'QPRINT'
     c                   eval      ORCOPY = 1
     c                   eval      ORTITL = 'POLICY PAGE 3C VBG: -
     c                                      POLICY VS GROUP'
     c                   eval      ORSAVE = '*YES'
     c                   eval      ORHOLD = '*NO'
     c                   eval      ORFORM = '*STD'
     c                   eval      ORSTAT = 'A'
     c                   eval      ORUSER = Q1USER
     c                   eval      ORPGM  = Q1PGM
     c                   eval      ORTIME = dstime
     c                   eval      ORDT   = sysDt
     c                   eval      OROFLD = 'OVRPRTF '        +
     c                                      'FILE(QSYSPRT) '  +
     c                                      'OUTQ(QPRINT) '   +
     c                                      'FORMTYPE(*STD) ' +
     c                                      'COPIES(01) '     +
     c                                      'HOLD(*NO)  '     +
     c                                      'SAVE(*YES)'
     c                   endif
     c                   endsr
