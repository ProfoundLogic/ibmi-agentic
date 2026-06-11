      *
      * Program: SCCVBR - Retrieve policy year to be used in naming of PDF
      *
      * REVISIONS:
      *
      * 01/07/08   C:180         CI055 - Customize year retrieval based on
      *                          client.
      *
      * 12/28/07   C:180         CI055 - Create policy page PDFs
      *
     FSPLDTA    IF   E           K DISK
     FWMRPL3    IF   E           K DISK
     D KEY1            C                   '        Policy #:'
     D KEY2            C                   'Carrier Policy #:'
     D KEY3            C                   '       Policy No.'
     D DIGITS          C                   '0123456789'

     D                 ds
     D qckey                         40
     D  qcco#                         3  0 overlay(qckey)
     D  qcfnd                         3  0 overlay(qckey:4)
     D  qcemp#                        9  0 overlay(qckey:7)
     D  qcdiv                         5  0 overlay(qckey:16)

     D                 ds
     D SPLFLD                        16
     D   SPLFLD3                           OVERLAY(SPLFLD:1)
     D   SPLFLD4                           OVERLAY(SPLFLD:14)
     D   SPLFLD3B                    15    OVERLAY(SPLFLD:2)
     D   SPLFLD3C                     3    OVERLAY(SPLFLD3B:13)
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    YEAR              3
     C                   PARM                    RTCD              1

     C     KYWMRP        KLIST
     C                   KFLD                    QCCO#
     C                   KFLD                    QCCARR
     C                   KFLD                    QCFND
     C                   KFLD                    QCEMP#
     C                   KFLD                    QCDIV
      *
      *
     C                   MOVE      QQKEY         QCKEY
     C                   MOVE      *ZEROS        YEAR
     C                   MOVE      '1'           RTCD

      ********* BEGIN ADD ************* 01/17/08 ***************************
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       CLTID             3

     C                   SELECT
     C                   WHEN      CLTID = '132'
     C                   EXSR      CHEK1
     C                   WHEN      CLTID = '136'
     C                   EXSR      CHEK1
     C                   IF        RTCD > *BLANK
     C                   EXSR      CHEK2
     C                   ENDIF
     C                   ENDSL
     C                   EVAL      *INLR = *ON
      ********* END ADD  ************** 01/17/08 ***************************

      ********* BEGIN ADD ************* 01/17/08 ***************************
     C     CHEK1         BEGSR
      ********* END ADD *************** 01/17/08 ***************************
     C     KEY1          CHAIN     SPLDTA
     C                   IF        %FOUND(SPLDTA)
     C                   MOVE      SPLFLD4       YEAR
     C                   MOVE      *BLANK        RTCD
     C                   ELSE
     C     KEY2          CHAIN     SPLDTA
     C                   IF        %FOUND(SPLDTA)
     C     DIGITS        CHECK     SPLFLD4
     C                   IF        NOT %FOUND and
     C                             %SUBST(SPLFLD3:11:1) <> '-'
     C                   MOVE      SPLFLD4       YEAR
     C                   MOVE      *BLANK        RTCD
     C                   ENDIF
      *
     C                   ELSE
     C     KEY3          CHAIN     SPLDTA
     C                   IF        %FOUND(SPLDTA) and
     C                             %SUBST(SPLFLD3B:12:1) = '-'
     C                   MOVEL(P)  SPLFLD3B      QCCARR           25
     C     KYWMRP        CHAIN     WMRPL3

     C                   IF        %FOUND(WMRPL3)
     C                   MOVE      RPFYR         YEAR
     C                   MOVE      *BLANK        RTCD
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      ********* BEGIN ADD ************* 01/17/08 ***************************
     C                   ENDSR

      *---------------------------------------------------------------------
     C     CHEK2         BEGSR
      *---------------------------------------------------------------------
     C     KEY3          CHAIN     SPLDTA
     C                   IF        %FOUND(SPLDTA)
     C     DIGITS        CHECK     SPLFLD3C
     C                   IF        NOT %FOUND and
     C                             %SUBST(SPLFLD3:12:1) <> '-'
     C                   MOVE      SPLFLD3C      YEAR
     C                   MOVE      *BLANK        RTCD
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
