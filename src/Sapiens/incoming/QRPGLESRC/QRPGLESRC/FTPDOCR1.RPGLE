     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h** ======================================================================================== **
    h/TITLE FTPDOCR1 - Update the FTP Instrcutions
     h option(*nodebugio:*srcstmt)
    h* ========================================================================================= **
    h*  DESCRIPTION:  Read the FTPINST source member                                              *
    h*                                                                                            *
     h*  Called from:                                                                              *
     h*                                                                                            *
     h*  Created: 06/25/09 by C:220 for project: 114WTR                                            *
    h** ======================================================================================== **
    h* Revisions:                                                                                 *
    h** ======================================================================================== **
     f*                                                                                            *
     f* TPA Contact Master Information (TPCO#/TPTPAC )
     fwmtcl     if   e           k disk
     f*                                                                                            *
     f* FTP file for commands
     fsourcef   uf a e             disk    rename(sourcef:p_sourcef)
     f*
      *=============================================================================================
      * Data structures
      *=============================================================================================
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
      *=============================================================================================
     d  temp0          s             20
     d  temp1          s             20
      *=============================================================================================
      * KLISTS
      *=============================================================================================
      *
     C     ktc01         klist
     C                   kfld                    @@co#
     C                   kfld                    @@tpacd
      *
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    @@co#             3 0
     c                   parm                    @@tpacd          10
     c                   parm                    @@docid           8
      *
      *=============================================================================================
      * Initialize
      *=============================================================================================
     c*
     c
     c*
      *=============================================================================================
      * Main loop
      *=============================================================================================
     c*
     c     ktc01         chain     ptc01                              60
     c                   if        *in60 = *off
     c                   dow       *in61 = *off
     c                   read      p_sourcef                              61
     c*
     c                   move      srcdta        xxcdta          140
     c*
     c     'USRPRF'      SCAN      xxcdta        x                 2 0
     c                   if        x <> 0
     c*
     c                   eval      xxcdta = (%trim(tcusid) + ' ' +
     c                             %trim(tcspsw))
     c*
     c                   movel     xxcdta        srcdta
     c*
     c                   update    p_sourcef
     c*
     c                   move      *blanks       xxcdta
     c*
     c                   endif
     c*
     c*  PUT LOCALDIRECTORY/LOCALDOCUMENT REMOTEDIECTORY/LOCALDOCUMENT
     c*
     c     'PUT'         SCAN      xxcdta        x                 2 0
     c                   if        x <> 0
     c*
     c*                  eval      xxcdta = ('PUT /ZEXPORT/' +
     c*                            %trim(@@docid) + ' ' + %trim(tcfldr)  + '/' +
     c*                            %trim(@@docid))
     c*
     c                   eval      xxcdta = ('PUT /ZEXPORT/' +
     c                             (@@docid) + ' ' + %trim(tcfldr)  + '/' +
     c                             (@@docid))
     c*
     c                   movel     xxcdta        srcdta
     c*
     c                   update    p_sourcef
     c*
     c                   move      *blanks       xxcdta
     c*
     c                   endif
     c*
     c                   enddo                                                  SRCMBR file
     c*
     c                   endif
     c*
     c*                                                                         TPA Contact Master-
     c*                                                                         Information
      * ===================================================================== *
     c                   eval      *inlr = *on
      * ===================================================================== *
