*DECK HWSSS1
      SUBROUTINE HWSSS1 (TS, TF, M, MBDCND, BDTS, BDTF, PS, PF, N,
     +   NBDCND, BDPS, BDPF, ELMBDA, F, IDIMF, PERTRB, AM, BM, CM, SN,
     +   SS, SINT, D)
C***BEGIN PROLOGUE  HWSSS1
C***SUBSIDIARY
C***PURPOSE  Subsidiary to HWSSSP
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (HWSSS1-S)
C***AUTHOR  (UNKNOWN)
C***SEE ALSO  HWSSSP
C***ROUTINES CALLED  GENBUN
C***REVISION HISTORY  (YYMMDD)
C   801001  DATE WRITTEN
C   891009  Removed unreferenced variables.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900402  Added TYPE section.  (WRB)
C***END PROLOGUE  HWSSS1
      DIMENSION       F(IDIMF,*) ,BDTS(*)    ,BDTF(*)    ,BDPS(*)    ,
     1                BDPF(*)    ,AM(*)      ,BM(*)      ,CM(*)      ,
     2                SS(*)      ,SN(*)      ,D(*)       ,SINT(*)
C
C***FIRST EXECUTABLE STATEMENT  HWSSS1
      MP1 = M+1
      NP1 = N+1
      FN = N
      FM = M
      DTH = (TF-TS)/FM
      HDTH = DTH/2.
      TDT = DTH+DTH
      DPHI = (PF-PS)/FN
      TDP = DPHI+DPHI
      DPHI2 = DPHI*DPHI
      DTH2 = DTH*DTH
      CP = 4./(FN*DTH2)
      WP = FN*SIN(HDTH)/4.
      DO 102 I=1,MP1
         FIM1 = I-1
         THETA = FIM1*DTH+TS
         SINT(I) = SIN(THETA)
         IF (SINT(I)) 101,102,101
  101    T1 = 1./(DTH2*SINT(I))
         AM(I) = T1*SIN(THETA-HDTH)
         CM(I) = T1*SIN(THETA+HDTH)
         BM(I) = -AM(I)-CM(I)+ELMBDA
  102 CONTINUE
      INP = 0
      ISP = 0
C
C BOUNDARY CONDITION AT THETA=TS
C
      MBR = MBDCND+1
      GO TO (103,104,104,105,105,106,106,104,105,106),MBR
  103 ITS = 1
      GO TO 107
  104 AT = AM(2)
      ITS = 2
      GO TO 107
  105 AT = AM(1)
      ITS = 1
      CM(1) = AM(1)+CM(1)
      GO TO 107
  106 AT = AM(2)
      INP = 1
      ITS = 2
C
C BOUNDARY CONDITION THETA=TF
C
  107 GO TO (108,109,110,110,109,109,110,111,111,111),MBR
  108 ITF = M
      GO TO 112
  109 CT = CM(M)
      ITF = M
      GO TO 112
  110 CT = CM(M+1)
      AM(M+1) = AM(M+1)+CM(M+1)
      ITF = M+1
      GO TO 112
  111 ITF = M
      ISP = 1
      CT = CM(M)
C
C COMPUTE HOMOGENEOUS SOLUTION WITH SOLUTION AT POLE EQUAL TO ONE
C
  112 ITSP = ITS+1
      ITFM = ITF-1
      WTS = SINT(ITS+1)*AM(ITS+1)/CM(ITS)
      WTF = SINT(ITF-1)*CM(ITF-1)/AM(ITF)
      MUNK = ITF-ITS+1
      IF (ISP) 116,116,113
  113 D(ITS) = CM(ITS)/BM(ITS)
      DO 114 I=ITSP,M
         D(I) = CM(I)/(BM(I)-AM(I)*D(I-1))
  114 CONTINUE
      SS(M) = -D(M)
      IID = M-ITS
      DO 115 II=1,IID
         I = M-II
         SS(I) = -D(I)*SS(I+1)
  115 CONTINUE
      SS(M+1) = 1.
  116 IF (INP) 120,120,117
  117 SN(1) = 1.
      D(ITF) = AM(ITF)/BM(ITF)
      IID = ITF-2
      DO 118 II=1,IID
         I = ITF-II
         D(I) = AM(I)/(BM(I)-CM(I)*D(I+1))
  118 CONTINUE
      SN(2) = -D(2)
      DO 119 I=3,ITF
         SN(I) = -D(I)*SN(I-1)
  119 CONTINUE
C
C BOUNDARY CONDITIONS AT PHI=PS
C
  120 NBR = NBDCND+1
      WPS = 1.
      WPF = 1.
      GO TO (121,122,122,123,123),NBR
  121 JPS = 1
      GO TO 124
  122 JPS = 2
      GO TO 124
  123 JPS = 1
      WPS = .5
C
C BOUNDARY CONDITION AT PHI=PF
C
  124 GO TO (125,126,127,127,126),NBR
  125 JPF = N
      GO TO 128
  126 JPF = N
      GO TO 128
  127 WPF = .5
      JPF = N+1
  128 JPSP = JPS+1
      JPFM = JPF-1
      NUNK = JPF-JPS+1
      FJJ = JPFM-JPSP+1
C
C SCALE COEFFICIENTS FOR SUBROUTINE GENBUN
C
      DO 129 I=ITS,ITF
         CF = DPHI2*SINT(I)*SINT(I)
         AM(I) = CF*AM(I)
         BM(I) = CF*BM(I)
         CM(I) = CF*CM(I)
  129 CONTINUE
      AM(ITS) = 0.
      CM(ITF) = 0.
      ISING = 0
      GO TO (130,138,138,130,138,138,130,138,130,130),MBR
  130 GO TO (131,138,138,131,138),NBR
  131 IF (ELMBDA) 138,132,132
  132 ISING = 1
      SUM = WTS*WPS+WTS*WPF+WTF*WPS+WTF*WPF
      IF (INP) 134,134,133
  133 SUM = SUM+WP
  134 IF (ISP) 136,136,135
  135 SUM = SUM+WP
  136 SUM1 = 0.
      DO 137 I=ITSP,ITFM
         SUM1 = SUM1+SINT(I)
  137 CONTINUE
      SUM = SUM+FJJ*(SUM1+WTS+WTF)
      SUM = SUM+(WPS+WPF)*SUM1
      HNE = SUM
  138 GO TO (146,142,142,144,144,139,139,142,144,139),MBR
  139 IF (NBDCND-3) 146,140,146
  140 YHLD = F(1,JPS)-4./(FN*DPHI*DTH2)*(BDPF(2)-BDPS(2))
      DO 141 J=1,NP1
         F(1,J) = YHLD
  141 CONTINUE
      GO TO 146
  142 DO 143 J=JPS,JPF
         F(2,J) = F(2,J)-AT*F(1,J)
  143 CONTINUE
      GO TO 146
  144 DO 145 J=JPS,JPF
         F(1,J) = F(1,J)+TDT*BDTS(J)*AT
  145 CONTINUE
  146 GO TO (154,150,152,152,150,150,152,147,147,147),MBR
  147 IF (NBDCND-3) 154,148,154
  148 YHLD = F(M+1,JPS)-4./(FN*DPHI*DTH2)*(BDPF(M)-BDPS(M))
      DO 149 J=1,NP1
         F(M+1,J) = YHLD
  149 CONTINUE
      GO TO 154
  150 DO 151 J=JPS,JPF
         F(M,J) = F(M,J)-CT*F(M+1,J)
  151 CONTINUE
      GO TO 154
  152 DO 153 J=JPS,JPF
         F(M+1,J) = F(M+1,J)-TDT*BDTF(J)*CT
  153 CONTINUE
  154 GO TO (159,155,155,157,157),NBR
  155 DO 156 I=ITS,ITF
         F(I,2) = F(I,2)-F(I,1)/(DPHI2*SINT(I)*SINT(I))
  156 CONTINUE
      GO TO 159
  157 DO 158 I=ITS,ITF
         F(I,1) = F(I,1)+TDP*BDPS(I)/(DPHI2*SINT(I)*SINT(I))
  158 CONTINUE
  159 GO TO (164,160,162,162,160),NBR
  160 DO 161 I=ITS,ITF
         F(I,N) = F(I,N)-F(I,N+1)/(DPHI2*SINT(I)*SINT(I))
  161 CONTINUE
      GO TO 164
  162 DO 163 I=ITS,ITF
         F(I,N+1) = F(I,N+1)-TDP*BDPF(I)/(DPHI2*SINT(I)*SINT(I))
  163 CONTINUE
  164 CONTINUE
      PERTRB = 0.
      IF (ISING) 165,176,165
  165 SUM = WTS*WPS*F(ITS,JPS)+WTS*WPF*F(ITS,JPF)+WTF*WPS*F(ITF,JPS)+
     1      WTF*WPF*F(ITF,JPF)
      IF (INP) 167,167,166
  166 SUM = SUM+WP*F(1,JPS)
  167 IF (ISP) 169,169,168
  168 SUM = SUM+WP*F(M+1,JPS)
  169 DO 171 I=ITSP,ITFM
         SUM1 = 0.
         DO 170 J=JPSP,JPFM
            SUM1 = SUM1+F(I,J)
  170    CONTINUE
         SUM = SUM+SINT(I)*SUM1
  171 CONTINUE
      SUM1 = 0.
      SUM2 = 0.
      DO 172 J=JPSP,JPFM
         SUM1 = SUM1+F(ITS,J)
         SUM2 = SUM2+F(ITF,J)
  172 CONTINUE
      SUM = SUM+WTS*SUM1+WTF*SUM2
      SUM1 = 0.
      SUM2 = 0.
      DO 173 I=ITSP,ITFM
         SUM1 = SUM1+SINT(I)*F(I,JPS)
         SUM2 = SUM2+SINT(I)*F(I,JPF)
  173 CONTINUE
      SUM = SUM+WPS*SUM1+WPF*SUM2
      PERTRB = SUM/HNE
      DO 175 J=1,NP1
         DO 174 I=1,MP1
            F(I,J) = F(I,J)-PERTRB
  174    CONTINUE
  175 CONTINUE
C
C SCALE RIGHT SIDE FOR SUBROUTINE GENBUN
C
  176 DO 178 I=ITS,ITF
         CF = DPHI2*SINT(I)*SINT(I)
         DO 177 J=JPS,JPF
            F(I,J) = CF*F(I,J)
  177    CONTINUE
  178 CONTINUE
      CALL GENBUN (NBDCND,NUNK,1,MUNK,AM(ITS),BM(ITS),CM(ITS),IDIMF,
     1             F(ITS,JPS),IERROR,D)
      IF (ISING) 186,186,179
  179 IF (INP) 183,183,180
  180 IF (ISP) 181,181,186
  181 DO 182 J=1,NP1
         F(1,J) = 0.
  182 CONTINUE
      GO TO 209
  183 IF (ISP) 186,186,184
  184 DO 185 J=1,NP1
         F(M+1,J) = 0.
  185 CONTINUE
      GO TO 209
  186 IF (INP) 193,193,187
  187 SUM = WPS*F(ITS,JPS)+WPF*F(ITS,JPF)
      DO 188 J=JPSP,JPFM
         SUM = SUM+F(ITS,J)
  188 CONTINUE
      DFN = CP*SUM
      DNN = CP*((WPS+WPF+FJJ)*(SN(2)-1.))+ELMBDA
      DSN = CP*(WPS+WPF+FJJ)*SN(M)
      IF (ISP) 189,189,194
  189 CNP = (F(1,1)-DFN)/DNN
      DO 191 I=ITS,ITF
         HLD = CNP*SN(I)
         DO 190 J=JPS,JPF
            F(I,J) = F(I,J)+HLD
  190    CONTINUE
  191 CONTINUE
      DO 192 J=1,NP1
         F(1,J) = CNP
  192 CONTINUE
      GO TO 209
  193 IF (ISP) 209,209,194
  194 SUM = WPS*F(ITF,JPS)+WPF*F(ITF,JPF)
      DO 195 J=JPSP,JPFM
         SUM = SUM+F(ITF,J)
  195 CONTINUE
      DFS = CP*SUM
      DSS = CP*((WPS+WPF+FJJ)*(SS(M)-1.))+ELMBDA
      DNS = CP*(WPS+WPF+FJJ)*SS(2)
      IF (INP) 196,196,200
  196 CSP = (F(M+1,1)-DFS)/DSS
      DO 198 I=ITS,ITF
         HLD = CSP*SS(I)
         DO 197 J=JPS,JPF
            F(I,J) = F(I,J)+HLD
  197    CONTINUE
  198 CONTINUE
      DO 199 J=1,NP1
         F(M+1,J) = CSP
  199 CONTINUE
      GO TO 209
  200 RTN = F(1,1)-DFN
      RTS = F(M+1,1)-DFS
      IF (ISING) 202,202,201
  201 CSP = 0.
      CNP = RTN/DNN
      GO TO 205
  202 IF (ABS(DNN)-ABS(DSN)) 204,204,203
  203 DEN = DSS-DNS*DSN/DNN
      RTS = RTS-RTN*DSN/DNN
      CSP = RTS/DEN
      CNP = (RTN-CSP*DNS)/DNN
      GO TO 205
  204 DEN = DNS-DSS*DNN/DSN
      RTN = RTN-RTS*DNN/DSN
      CSP = RTN/DEN
      CNP = (RTS-DSS*CSP)/DSN
  205 DO 207 I=ITS,ITF
         HLD = CNP*SN(I)+CSP*SS(I)
         DO 206 J=JPS,JPF
            F(I,J) = F(I,J)+HLD
  206    CONTINUE
  207 CONTINUE
      DO 208 J=1,NP1
         F(1,J) = CNP
         F(M+1,J) = CSP
  208 CONTINUE
  209 IF (NBDCND) 212,210,212
  210 DO 211 I=1,MP1
         F(I,JPF+1) = F(I,JPS)
  211 CONTINUE
  212 RETURN
      END
