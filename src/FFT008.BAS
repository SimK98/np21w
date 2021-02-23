1000 '----------------------------------------------------------------------
1010 ' BASICで高速フーリエ変換 第8回 三角波
1020 '
1030 ' for N88-BASIC 2021.01 by SimK
1090 '----------------------------------------------------------------------
1100 SCREEN 3: CLS 3: CONSOLE ,,,1
1200 '
1210 PI = 3.14159265 ' π
1220 FFTLEN% = 512 ' FFTデータ点数（2^nの値）
1230 SIGNALTYPE = 0 ' 入力信号のタイプ 0=実数信号, 1=複素信号 表示が変わるだけ
1250 COLOR 7: IF SIGNALTYPE=0 THEN PRINT "REAL SIGNAL" ELSE PRINT "COMPLEX SIGNAL"
1255 LOCATE 0,8:
1260 COLOR 7: PRINT "SPECTRUM ";: COLOR 6: PRINT "INTENSITY(0～MAX) ";: COLOR 5: PRINT "PHASE(-PI～PI) ";: COLOR 2: PRINT "PASSBAND"
1265 COLOR 7: LOCATE 0,16: PRINT "IFFT "
1270 LOCATE 0,21: COLOR 7: 
1280 GOSUB *FFTINIT ' FFT初期化
1290 DIM FFTDATA(FFTLEN%*2-1) ' FFTするデータ
1300 FOR I%=0 TO FFTLEN%-1 'FFTするデータを生成
1310   IF (I% \ 32) MOD 2 = 0 THEN FFTDATA(I%*2)=(I% MOD 32)/16-1 ELSE FFTDATA(I%*2)=(31-(I% MOD 32))/16-1
1320 NEXT
1410 NORMVAL=0: GOSUB *FFTNORMALIZE ' 最大値1になるように正規化
1420 GX%=32: GY%= 24: GW%=FFTLEN%: GH%=96: IF SIGNALTYPE=1 THEN GOSUB *DRAWDATA ELSE GOSUB *DRAWDATARE ' 変換前グラフ描画
1430 FFTDIR%=1: GOSUB *FFT: ' FFT実行
1440 NORMVAL=0: GOSUB *FFTNORMALIZE ' 最大値1になるように正規化
1450 GX%=32: GY%=150: GW%=FFTLEN%: GH%=96: GOSUB *DRAWDATA ' 変換後グラフ描画
1500 LPFTH%=32 'ローパスフィルタ
1510 FOR I%=LPFTH% TO FFTLEN%\2
1520   FFTDATA((I%)*2)=0: FFTDATA((I%)*2+1)=0: 
1530   FFTDATA((FFTLEN%-I%)*2)=0: FFTDATA((FFTLEN%-I%)*2+1)=0
1540 NEXT
1560 LINE(GX%,GY%)-(GX%+LPFTH%*GW%/FFTLEN%,GY%+GH%-1),2,B ' 通過範囲図示
1565 LINE(GX%+GW%-LPFTH%*GW%/FFTLEN%-1,GY%)-(GX%+GW%-1,GY%+GH%-1),2,B ' 通過範囲図示
1660 FFTDIR%=-1: GOSUB *FFT: ' IFFT実行
1670 NORMVAL=0: GOSUB *FFTNORMALIZE ' 最大値1になるように正規化
1680 GX%=32: GY%= 280: GW%=FFTLEN%: GH%=96: IF SIGNALTYPE=1 THEN GOSUB *DRAWDATA ELSE GOSUB *DRAWDATARE ' IFFT後グラフ描画
1900 END
3000 '----------------------------------------------------------------------
3010 ' データ実部をグラフとして描く
3020 ' inp FFTLEN% データ長さ, FFTDATA データ,
3030 '     GX% 基点X, GY% 基点Y, GW% 幅, GH% 高さ
3090 '----------------------------------------------------------------------
3100 *DRAWDATARE
3110 LINE(GX%-1,GY%-1)-(GX%+GW%,GY%+GH%),7,B ' 枠
3120 D1 = FFTDATA(0)
3130 FOR I%=1 TO FFTLEN%-1
3140   SX% = GX% + (I%-1)*GW%/FFTLEN%
3150   EX% = GX% + I%*GW%/FFTLEN%
3160   D2 = FFTDATA(I%*2)
3200   SY% = GY% + INT(-D1*GH%/2) + GH%/2
3210   EY% = GY% + INT(-D2*GH%/2) + GH%/2
3220   LINE(SX%,SY%)-(EX%,EY%),7
3230   D1 = D2:
3240 NEXT
3250 RETURN
4000 '----------------------------------------------------------------------
4010 ' データを強度+位相グラフとして描く
4020 ' inp FFTLEN% データ長さ, FFTDATA データ,
4030 '     GX% 基点X, GY% 基点Y, GW% 幅, GH% 高さ
4090 '----------------------------------------------------------------------
4100 *DRAWDATA
4110 LINE(GX%-1,GY%-1)-(GX%+GW%,GY%+GH%),7,B ' 枠
4120 D1 = FFTDATA(0)^2 + FFTDATA(1)^2
4125 IF FFTDATA(0)=0 THEN P1=0 ELSE P1 = ATN(FFTDATA(1)/FFTDATA(0)): IF FFTDATA(0)<0 THEN IF P1>0 THEN P1=P1-PI ELSE P1=P1+PI
4130 FOR I%=1 TO FFTLEN%-1
4140   SX% = GX% + (I%-1)*GW%/FFTLEN%
4150   EX% = GX% + I%*GW%/FFTLEN%
4160   D2 = FFTDATA(I%*2)^2+FFTDATA(I%*2+1)^2
4165   IF FFTDATA(I%*2)=0 THEN P2=0 ELSE P2 = ATN(FFTDATA(I%*2+1)/FFTDATA(I%*2)): IF FFTDATA(I%*2)<0 THEN IF P2>0 THEN P2=P2-PI ELSE P2=P2+PI
4170   SY% = GY% + INT(-P1*GH%/2/PI) + GH%/2
4180   EY% = GY% + INT(-P2*GH%/2/PI) + GH%/2
4190   LINE(SX%,SY%)-(EX%,EY%),5 ' 位相
4200   SY% = GY% + INT((1-D1)*GH%)
4210   EY% = GY% + INT((1-D2)*GH%)
4220   LINE(SX%,SY%)-(EX%,EY%),6 ' 強度
4230   D1 = D2: P1 = P2
4240 NEXT
4250 RETURN
5000 '----------------------------------------------------------------------
5010 ' 高速フーリエ変換用 三角関数テーブルを作成する
5020 ' inp FFTLEN%  FFT長さ
5030 ' out SINTBL 三角関数テーブル
5040 ' tmp I%, TBLOFS%
5050 '----------------------------------------------------------------------
5100 *FFTINIT
5110 TBLOFS% = FFTLEN% \ 4
5120 DIM SINTBL(FFTLEN%+TBLOFS%)
5130 FOR I%=0 TO FFTLEN%+TBLOFS%
5140   SINTBL(I%) = SIN(I%*2*PI/FFTLEN%)
5150 NEXT
5160 RETURN
6000 '----------------------------------------------------------------------
6010 ' 高速フーリエ変換実行 
6020 ' http://www.kurims.kyoto-u.ac.jp/~ooura/fftman/ftmn1_2.html
6030 ' inp FFTLEN% FFT長さ, FFTDATA データ, FFTDIR% 変換方向(+1:順, -1:逆)
6040 ' out FFTDATA データ(in-place)
6050 ' tmp I%, J%, K%, TBLOFS%, EXPBASE%, M%, MH%, WR, WI, XR, XI
6060 '----------------------------------------------------------------------
6100 *FFT
6110 TBLOFS% = FFTLEN% \ 4
6120 EXPBASE% = FFTDIR%
6125 IF FFTDIR%=-1 THEN EXPOFS%=FFTLEN% ELSE EXPOFS%=0
6130 M% = FFTLEN%
6140 WHILE M%>1
6150   MH% = M% \ 2
6160   FOR I%=0 TO MH%-1
6170     WR = SINTBL(TBLOFS% + EXPOFS% + EXPBASE% * I%)
6180     WI = SINTBL(EXPOFS% + EXPBASE% * I%)
6190     FOR J%=I% TO FFTLEN%-1 STEP M%
6200       K% = J% + MH%
6210       XR = FFTDATA(J%*2  ) - FFTDATA(K%*2  )
6220       XI = FFTDATA(J%*2+1) - FFTDATA(K%*2+1)
6230       FFTDATA(J%*2  ) = FFTDATA(J%*2  ) + FFTDATA(K%*2  )
6240       FFTDATA(J%*2+1) = FFTDATA(J%*2+1) + FFTDATA(K%*2+1)
6250       FFTDATA(K%*2  ) = WR*XR - WI*XI
6260       FFTDATA(K%*2+1) = WR*XI + WI*XR
6270     NEXT
6280   NEXT
6290   EXPBASE% = EXPBASE% * 2
6300   M% = MH%
6310 WEND
6320   I% = 0
6330 FOR J%=1 TO FFTLEN%-2
6340   K% = FFTLEN% \ 2
6350   I% = I% XOR K%
6360   WHILE K%>I%
6370     K% = K% \ 2
6380     I% = I% XOR K%
6390   WEND
6400   IF J%>=I% THEN GOTO 6470
6410     XR = FFTDATA(J%*2  )
6420     XI = FFTDATA(J%*2+1)
6430     FFTDATA(J%*2  ) = FFTDATA(I%*2  )
6440     FFTDATA(J%*2+1) = FFTDATA(I%*2+1)
6450     FFTDATA(I%*2  ) = XR
6460     FFTDATA(I%*2+1) = XI
6470 NEXT
6480 RETURN
7000 '----------------------------------------------------------------------
7010 ' データ正規化
7020 ' inp FFTLEN% FFT長さ, FFTDATA データ(in-place), 
7030 '     NORMVAL 正規化係数 0なら最大値を使用
7040 ' out FFTDATA データ(in-place)
7050 ' tmp I%, D
7060 '----------------------------------------------------------------------
7100 *FFTNORMALIZE
7110 IF NORMVAL<>0 GOTO 7160
7120   FOR I%=0 TO FFTLEN%-1
7130     D = SQR(FFTDATA(I%*2)^2+FFTDATA(I%*2+1)^2)
7140     IF D>NORMVAL THEN NORMVAL = D
7150   NEXT
7160   IF NORMVAL=0 THEN RETURN
7170 FOR I%=0 TO FFTLEN%*2-1
7180   FFTDATA(I%) = FFTDATA(I%) / NORMVAL
7190 NEXT
7200 RETURN
