     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2002                   *
     *========================================================================*
    H/TITLE SWQ05R - WC Quote Employer Info for Web Display
     *========================================================================*
     * PROGRAM DESCRIPTION:                                                   *
     *  Returns Employer quote info to display on web page.                .  *
      *
     * CALLED BY : CGI programs for Web                                       *
     *                                                                        *
     * CALLS  TO : None                                                       *
     *                                                                        *
     * CREATED   : 08/22/2002                                                 *
     * PROGRAMMER: C:145                                                      *
     * PROJECT   : DR176                                                      *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * XX/XX/XXXX C:XXX PROJECT: XXXXX                                        *
     *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
      *
     *========================================================================*
     * Stand-alone Field Definitions
     *========================================================================*
      *
     DiDrtKey          s             40a
     DiPnam            s             10a
     DvHead1           s             40a
     DvHead2           s             40a
     DvHead3a          s             40a
     DvHead3b          s             40a
     DvDHd01           s             40a
     DvDl1c1           s             40a
     DvDl1c2           s             40a
     DvDl1c3           s             40a
     DvDl1c4           s             40a
     DvDl2c1           s             40a
     DvDl2c2           s             40a
     DvDl2c3           s             40a
     DvDl2c4           s             40a
     DvDl3c1           s             40a
     DvDl3c2           s             40a
     DvDl3c3           s             40a
     DvDl3c4           s             40a
     DvDl4c1           s             40a
     DvDl4c2           s             40a
     DvDl4c3           s             40a
     DvDl4c4           s             40a
     DvDl5c1           s             40a
     DvDl5c2           s             40a
     DvDl5c3           s             40a
     DvDl5c4           s             40a
     DvDl6c1           s             40a
     DvDl6c2           s             40a
     DvDl6c3           s             40a
     DvDl6c4           s             40a
     DvDl7c1           s             40a
     DvDl7c2           s             40a
     DvDl8c1           s             40a
     DvDl8c2           s             40a
     DvDl8c3           s             40a
     DvDl8c4           s             40a
     DvDl9c1           s             40a
     DvDl9c2           s             40a
     DvDl9c3           s             40a
     DvDl9c4           s             40a
     DvRtcd            s              2a
     Dadd              s              1a
      *
      *
     *========================================================================*
     * PLIST Definitions
     *========================================================================*
      *
     C     *entry        PList
     C                   parm                    idrtkey
     C                   parm                    ipnam
     C                   parm                    vHead1
     C                   parm                    vHead2
     C                   parm                    vHead3a
     C                   parm                    vHead3b
     C                   parm                    vDHd01
     C                   parm                    vDl1c1
     C                   parm                    vDl1c2
     C                   parm                    vDl1c3
     C                   parm                    vDl1c4
     C                   parm                    vDl2c1
     C                   parm                    vDl2c2
     C                   parm                    vDl2c3
     C                   parm                    vDl2c4
     C                   parm                    vDl3c1
     C                   parm                    vDl3c2
     C                   parm                    vDl3c3
     C                   parm                    vDl3c4
     C                   parm                    vDl4c1
     C                   parm                    vDl4c2
     C                   parm                    vDl4c3
     C                   parm                    vDl4c4
     C                   parm                    vDl5c1
     C                   parm                    vDl5c2
     C                   parm                    vDl5c3
     C                   parm                    vDl5c4
     C                   parm                    vDl6c1
     C                   parm                    vDl6c2
     C                   parm                    vDl6c3
     C                   parm                    vDl6c4
     C                   parm                    vDl7c1
     C                   parm                    vDl7c2
     C                   parm                    vDl8c1
     C                   parm                    vDl8c2
     C                   parm                    vDl8c3
     C                   parm                    vDl8c4
     C                   parm                    vDl9c1
     C                   parm                    vDl9c2
     C                   parm                    vDl9c3
     C                   parm                    vDl9c4
     C                   parm                    vRtcd
     *========================================================================*
     * Main
     *========================================================================*
     C
     C                   exsr      sbr
     C                   eval      *inlr = *on
     C
     *========================================================================*
     * Start Subroutine
     *========================================================================*
     C     sbr           begsr
     C*
     C****  Call a control program to set up the library list
     C                   eval      add = '1'
     C                   call      'SWQ05C'
     C                   parm                    add
     C
     C                   call      'SWQ05R1'
     C                   parm                    idrtkey
     C                   parm                    ipnam
     C                   parm                    vhead1
     C                   parm                    vhead2
     C                   parm                    vhead3a
     C                   parm                    vhead3b
     C                   parm                    vdhd01
     C                   parm                    vdl1c1
     C                   parm                    vdl1c2
     C                   parm                    vdl1c3
     C                   parm                    vdl1c4
     C                   parm                    vdl2c1
     C                   parm                    vdl2c2
     C                   parm                    vdl2c3
     C                   parm                    vdl2c4
     C                   parm                    vdl3c1
     C                   parm                    vdl3c2
     C                   parm                    vdl3c3
     C                   parm                    vdl3c4
     C                   parm                    vdl4c1
     C                   parm                    vdl4c2
     C                   parm                    vdl4c3
     C                   parm                    vdl4c4
     C                   parm                    vdl5c1
     C                   parm                    vdl5c2
     C                   parm                    vdl5c3
     C                   parm                    vdl5c4
     C                   parm                    vdl6c1
     C                   parm                    vdl6c2
     C                   parm                    vdl6c3
     C                   parm                    vdl6c4
     C                   parm                    vdl7c1
     C                   parm                    vdl7c2
     C                   parm                    vdl8c1
     C                   parm                    vdl8c2
     C                   parm                    vdl8c3
     C                   parm                    vdl8c4
     C                   parm                    vdl9c1
     C                   parm                    vdl9c2
     C                   parm                    vdl9c3
     C                   parm                    vdl9c4
     C                   parm                    vrtcd
     C*
     C****  Call a control program to remove the library list
     C                   eval      add = '0'
     C                   call      'SWQ05C'
     C                   parm                    add
     C
     C                   endsr
