     D*=====================================================================
     D* Contact procedure prototypes
     D*=====================================================================
    D*
    P*====================================================================
    P* GetEmpEmail  - Get contact email address
    P*
    P*
    P*   co#      - passed co#                (required)
    P*   fnd      - passed fnd                (required)
    P*   emp#     - passed emp#               (required)
    P*   div      - passed div#               (required)
    P*   pcnt     - Primary Contact Designation
    P*
    P*   Returns
    P*   Email    - E mail address
    P*
     DGetEmpEmail      PR            60
     D co#                            3  0                  const
     D fnd                            3  0                  const
     D emp#                           9  0                  const
     D div                            5  0                  const
     D pcnt                           1                     const
