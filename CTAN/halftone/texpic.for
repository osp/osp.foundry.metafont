C --- -----------------------------------------------------------------
C ---                          T e X P I C
C --- -----------------------------------------------------------------
C
C --- These routines are used to generate an input file to TeX which,
C --- when processed, gives a halftone representation of the grey-level
C --- image. TEXPIC is the main picture-plotting routine: it requires a
C --- M x N pixel array ARRAY. The TeX commands are written to the file
C --- FN. These routines are taken from a general-purpose library of
C --- image processing software developed by the author, which explains
C --- why most of the variables in the COMMON blocks are not used. The
C --- Fortran is also machine-generated, which may account for some odd
C --- line breaks in the code. (No line, even in comments, has more
C --- than 72 characters in it.)
C
C --- The image representation used here conforms to the one adopted by
C --- the Numerical Algorithms Group (NAG) for their ``Image Processing
C --- Algorithm Library'' IPAL, although the coding does not  (it  does
C --- not allow a sub-region to be plotted).
C
C --- There are two versions of TEXPIC in this file: the first is VAX-
C --- specific, while the second should be fairly portable. Note that
C --- both routines declare the BLOCK DATA module ALGINI as EXTERNAL;
C --- this usually forces the linker to build it into executable files.
C
C --- TEXPIC's support routines are:
C
C ---    TEXMAX  set the maximum pixel width across the page
C ---    ZRANGE  fix the contrast for subsequent TEXPIC calls
C ---    ZAUTO   subsequent pictures have their contrast determined
C ---            from the data
C ---    ZSAME   subsequent pictures are plotted with the same
C ---            contrast as the previous one
C ---    DOPOS   subsequent pix have low pixel values plotted black
C ---    DONEG   subsequent pix have low pixel values plotted white
C ---    MINMAX  determines the range of the data
C ---    ALGERR  outputs error messages
C ---    ABANDN  VAX-specific ^C trap routine
C ---    ALGINI  block data module
C
C --- Details of the invocations are given in the comments associated
C --- with each routine. There is also a separate document which gives
C --- user-level documentation and examples. This is available as part
C --- of the ``VAX/VMS TeX User's Guide'', written by the author, or as
C --- a separate document.
C
C --- As supplied, TEXPIC uses a three-point contextual bilinear method
C --- to interpolate between pixels. The results it produces should be
C --- marginally better than using standard four-point interpolation;
C --- however, the author can detect no difference. If you'd prefer to
C --- use four-point interpolation, the line to change is marked in the
C --- TEXPIC source code.
C
C --- Since you get TEXPIC free of charge, there is no formal guarantee
C --- given by Essex University OR the author that the software works
C --- or that the documentation agrees with the code. Nevertheless, the
C --- author would be pleased to hear of any problems.
C
C --- TEXPIC and associated routines were written by:
C
C ---    Dr. Adrian F. Clark  (``Alien'')
C --- of Department of Electronic Systems Engineering
C ---    University of Essex
C ---    Wivenhoe Park
C ---    Colchester
C ---    Essex C04 3SQ
C ---    United Kingdom
C ---    Tel: Colchester (0206) 872432 (direct)
C ---    JANET: user ALIEN @UK.AC.ESSEX.ESE
C
C --- If you write, please mark the envelope with ``TeX''.
C
C --- Acknowledgements in any published work that uses TEXPIC would be
C --- appreciated.
C
C --- ENJOY!
C
C --- -----------------------------------------------------------------
      SUBROUTINE TEXPIC( ARRAY, M, N, FN )
C --- -----------------------------------------------------------------
C
C --- TEXPIC version 0.1 was written by Alien in Fortran-77.
C
C --- This  routine  writes out the M x N image ARRAY into the file FN
C --- in a form which is suitable for insertion into a  TeX  document.
C --- If FN has no filetype (``extension''), .TEX is used.
C
C ---    By  default,  the range of the data is determined and used to
C --- maximise  the  contrast  of  the  output  image.  This  can   be
C --- overridden  by  pre-setting the range of data values with a call
C --- to ZRANGE. ZAUTO  restores  the  default  behaviour.  Similarly,
C --- TEXPIC  will  produce  negated  images  on  output  if DONEG has
C --- previously been invoked.  DOPOS  sets  it  to  produce  positive
C --- pictures again.
C
C --- USAGE:    CALL TEXPIC( ARRAY, M, N, FN )
C
C --- PARAMETERS
C ---     ARRAY  REAL           image to be output to the file
C ---     M      INTEGER        first dimension of ARRAY
C ---     N      INTEGER        second dimension of ARRAY
C ---     FN     CHARACTER*(*)  name  of  file  to  which ARRAY will
C ---                           be written
C
C --- RESTRICTIONS
C --- If N is greater than MMAX, the image will be sub-sampled in both
C --- directions  to  make  the result  MMAX x MMAX. The interpolation
C --- technique used is due to P.R. Smith (Ultramicroscopy vol  6,  pp
C --- 201--204, 1981).
C
C --- COMMONS
C --- /ALG/, /ALGTEX/
C
C --- SUBPROGRAMS INVOKED
C --- MINMAX, LIB$GET_LUN, LIB$FREE_LUN
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      INTEGER LEVELS, MINIDX, CMAX
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
      PARAMETER( LEVELS=65, MINIDX=48, CMAX=132 )
C
      CHARACTER*(*) FN
      INTEGER M, N
      REAL ARRAY(M,N)
C
      CHARACTER*(CMAX) C
      CHARACTER*6 RUTNAM
      INTEGER NMAX, I, J, IC, IV, LUN, IOS, ILO, JLO, IHI, JHI
      INTEGER LIB$GET_LUN, LIB$FREE_LUN
      LOGICAL POS
      REAL RANGE, INC
      REAL X, Y, DX, DY, DX1, DY1, VAL
C
      INTEGER*2 CHAN
      LOGICAL ABFLAG
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
      INTEGER MMAX
C
      COMMON /ALG_ABANDN/ ABFLAG, CHAN
      SAVE /ALG_ABANDN/
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
      COMMON/ALGTEX/ MMAX
      SAVE /ALGTEX/
      EXTERNAL ALGINI
      DATA RUTNAM/'TEXPIC'/
C
C --- Find the range of the data if we're in auto mode; otherwise,
C --- use the specified ranges.
C
      IF( .NOT. ZFIX ) THEN
         CALL MINMAX( ARRAY, M, N, ZMIN, ZMAX )
         IF( ZMIN .EQ. ZMAX ) ZMAX = ZMIN + 1
      END IF
C
C --- We can't print more than MMAX columns across the TeX output.
C --- If the user passes an array bigger than this, we'll interpolate
C --- it down to MMAX.
C
      IF( M .GT. MMAX ) THEN
         INC = FLOAT(M) / FLOAT(MMAX)
         NMAX = NINT( FLOAT(N) / INC )
      ELSE
         INC = 0
      END IF
C
C --- Get a free channel number and open the output file.
C
      IOS = LIB$GET_LUN( LUN )
      IF( .NOT. IOS ) CALL EXIT( IOS )
C
      OPEN( UNIT=LUN, FILE=FN, STATUS='NEW', RECL=CMAX+1, IOSTAT=IOS,
     & DEFAULTFILE='.TEX', CARRIAGECONTROL='LIST' )
      IF( IOS .EQ. 0 ) THEN
C
C --- Calculate the scaling factor.
C
         POS = .NOT. NEG
         RANGE = FLOAT(LEVELS-1) / (ZMAX-ZMIN)
C
C --- Output the introduction.
C
         WRITE( LUN, 100 )
C
C --- Output the image without interpolation if INC is zero.
C
         IF( ABS(INC) .LT. TOL ) THEN
            DO 2 J = 1, N
      IF( ABFLAG ) GO TO 5
               IC = 1
               C(1:1) = ','
               DO 1 I = 1, M
                  VAL = ARRAY(I,J)
               IF( VAL .LT. ZMIN ) VAL = ZMIN
               IF( VAL .GT. ZMAX ) VAL = ZMAX
               IV = NINT((VAL-ZMIN) * RANGE)
               IF( POS ) IV = (LEVELS-1) - IV
               IC = IC + 1
               C(IC:IC) = CHAR( IV + MINIDX )
               IF( IC .GE. CMAX-1 ) THEN
                  WRITE( LUN, 200 ) C(1:IC)
                  IC = 1
                  C(1:1) = ' '
               END IF
    1          CONTINUE
               IF( IC .GT. 0 ) WRITE(LUN, 300) C(1:IC)
    2       CONTINUE
         ELSE
C
C --- Interpolate the output.
C
            Y = 1
            DO 4 J = 1, NMAX
      IF( ABFLAG ) GO TO 5
               DY = Y - INT(Y)
               DY1 = 1 - DY
               JLO = MOD( INT(Y-1), N ) + 1
               JHI = MOD( JLO, N ) + 1
               X = 1
               IC = 1
               C(1:1) = ','
               DO 3 I = 1, MMAX
                  DX = X - INT(X)
                  DX1 = 1 - DX
                  ILO = MOD( INT(X)-1, M ) + 1
                  IHI = MOD( ILO, M ) + 1
C
C --- Smith's three-point contextual bilinear interpolation.
C
                  IF( ABS(ARRAY(ILO,JLO)-ARRAY(IHI,JHI)) .GT.
     &               ABS(ARRAY(IHI,JLO)-ARRAY(ILO,JHI)) ) THEN
                     VAL = (DX-DY)*ARRAY(IHI,JLO) + DX1*ARRAY(ILO,JLO) +
     &                  DY*ARRAY(IHI,JHI)
                  ELSE
                     VAL = (DX1-DY)*ARRAY(ILO,JLO) + DX*ARRAY(IHI,JLO) +
     &                  DY*ARRAY(ILO,JHI)
                  END IF
                  X = X + INC
               IF( VAL .LT. ZMIN ) VAL = ZMIN
               IF( VAL .GT. ZMAX ) VAL = ZMAX
               IV = NINT((VAL-ZMIN) * RANGE)
               IF( POS ) IV = (LEVELS-1) - IV
               IC = IC + 1
               C(IC:IC) = CHAR( IV + MINIDX )
               IF( IC .GE. CMAX-1 ) THEN
                  WRITE( LUN, 200 ) C(1:IC)
                  IC = 1
                  C(1:1) = ' '
               END IF
    3          CONTINUE
               IF( IC .GT. 0 ) WRITE(LUN, 300) C(1:IC)
C
               Y = Y + INC
    4       CONTINUE
         END IF
C
C --- Close off the file.
C
    5    CONTINUE
         WRITE( LUN, 400 )
         CLOSE( UNIT=LUN )
      ELSE
         CALL ALGERR( RUTNAM, 'Cannot open specified output file:',
     &    FN )
      END IF
C
C --- Release the channel.
C
      IOS = LIB$FREE_LUN( LUN )
      IF( .NOT. IOS ) CALL EXIT( IOS )
C
      RETURN
  100 FORMAT(' \hbox{\vbox{\halftone\offinterlineskip ',
     & '% machine-generated by TEXPIC.'/
     &  ' \def\BHT{\hbox\bgroup\ignorespaces}'/
     &  ' \catcode`\^=12 \catcode`\_=12 \catcode`\.=\active',
     &  ' \let.=\egroup'/ ' \catcode`\,=\active \let,=\BHT',
     &  ' \catcode`\/=0 \catcode`\\=12')
  200 FORMAT(1X,A,'%')
  300 FORMAT(1X,A,'.')
  400 FORMAT(' }}%')
      END
C --- -----------------------------------------------------------------
      SUBROUTINE TEXPIC( ARRAY, M, N, FN )
C --- -----------------------------------------------------------------
C
C --- TEXPIC version 0.1 was written by Alien in Fortran-77.
C
C --- This  routine  writes out the M x N image ARRAY into the file FN
C --- in a form which is suitable for insertion into a  TeX  document.
C
C ---    By  default,  the range of the data is determined and used to
C --- maximise  the  contrast  of  the  output  image.  This  can   be
C --- overridden  by  pre-setting the range of data values with a call
C --- to ZRANGE. ZAUTO  restores  the  default  behaviour.  Similarly,
C --- TEXPIC  will  produce  negated  images  on  output  if DONEG has
C --- previously been invoked.  DOPOS  sets  it  to  produce  positive
C --- pictures again.
C
C --- USAGE:    CALL TEXPIC( ARRAY, M, N, FN )
C
C --- PARAMETERS
C ---     ARRAY  REAL           image to be output to the file
C ---     M      INTEGER        first dimension of ARRAY
C ---     N      INTEGER        second dimension of ARRAY
C ---     FN     CHARACTER*(*)  name  of  file  to  which ARRAY will
C ---                           be written
C
C --- RESTRICTIONS
C --- If N is greater than MMAX, the image will be sub-sampled in both
C --- directions  to  make  the  result MMAX x MMAX. The interpolation
C --- technique used is due to P.R. Smith (Ultramicroscopy vol  6,  pp
C --- 201--204, 1981).
C
C --- COMMONS
C --- /ALG/, /ALGTEX/
C
C --- SUBPROGRAMS INVOKED
C --- MINMAX
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT CHARACTER*1 (A-Z)
C
      INTEGER LEVELS, MINIDX, CMAX
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
      PARAMETER( LEVELS=65, MINIDX=48, CMAX=132 )
C
      CHARACTER*(*) FN
      INTEGER M, N
      REAL ARRAY(M,N)
C
      CHARACTER*(CMAX) C
      CHARACTER*6 RUTNAM
      INTEGER NMAX, I, J, IC, IV, LUN, IOS, ILO, JLO, IHI, JHI
      LOGICAL POS
      REAL RANGE, INC
      REAL X, Y, DX, DY, DX1, DY1, VAL
C
      LOGICAL ABANDN
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
      INTEGER MMAX
C
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
      COMMON/ALGTEX/ MMAX
      SAVE /ALGTEX/
      EXTERNAL ALGINI
      DATA RUTNAM/'TEXPIC'/
C
C --- Find the range of the data if we're in auto mode; otherwise,
C --- use the specified ranges.
C
      IF( .NOT. ZFIX ) THEN
         CALL MINMAX( ARRAY, M, N, ZMIN, ZMAX )
         IF( ZMIN .EQ. ZMAX ) ZMAX = ZMIN + 1
      END IF
C
C --- We can't print more than MMAX columns across the TeX output.
C --- If the user passes an array bigger than this, we'll interpolate
C --- it down to MMAX.
C
      IF( M .GT. MMAX ) THEN
         INC = FLOAT(M) / FLOAT(MMAX)
         NMAX = NINT( FLOAT(N) / INC )
      ELSE
         INC = 0
      END IF
C
C --- We always open the output file on channel 7 (fix me!).
C
      LUN = 7
      OPEN( UNIT=LUN, FILE=FN, STATUS='NEW', RECL=CMAX+1, IOSTAT=IOS )
      IF( IOS .EQ. 0 ) THEN
C
C --- Calculate the scaling factor.
C
         POS = .NOT. NEG
         RANGE = FLOAT(LEVELS-1) / (ZMAX-ZMIN)
C
C --- Output the introduction.
C
         WRITE( LUN, 100 )
C
C --- Output the image without interpolation if INC is zero.
C
         IF( ABS(INC) .LT. TOL ) THEN
            DO 2 J = 1, N
      IF( ABANDN(0) ) GO TO 5
               IC = 1
               C(1:1) = ','
               DO 1 I = 1, M
                  VAL = ARRAY(I,J)
               IF( VAL .LT. ZMIN ) VAL = ZMIN
               IF( VAL .GT. ZMAX ) VAL = ZMAX
               IV = NINT((VAL-ZMIN) * RANGE)
               IF( POS ) IV = (LEVELS-1) - IV
               IC = IC + 1
               C(IC:IC) = CHAR( IV + MINIDX )
               IF( IC .GE. CMAX-1 ) THEN
                  WRITE( LUN, 200 ) C(1:IC)
                  IC = 1
                  C(1:1) = ' '
               END IF
    1          CONTINUE
               IF( IC .GT. 0 ) WRITE(LUN, 300) C(1:IC)
    2       CONTINUE
         ELSE
C
C --- Interpolate the output.
C
            Y = 1
            DO 4 J = 1, NMAX
      IF( ABANDN(0) ) GO TO 5
               DY = Y - INT(Y)
               DY1 = 1 - DY
               JLO = MOD( INT(Y-1), N ) + 1
               JHI = MOD( JLO, N ) + 1
               X = 1
               IC = 1
               C(1:1) = ','
               DO 3 I = 1, MMAX
                  DX = X - INT(X)
                  DX1 = 1 - DX
                  ILO = MOD( INT(X)-1, M ) + 1
                  IHI = MOD( ILO, M ) + 1
C
C --- Smith's three-point contextual bilinear interpolation.
C
                  IF( ABS(ARRAY(ILO,JLO)-ARRAY(IHI,JHI)) .GT.
     &               ABS(ARRAY(IHI,JLO)-ARRAY(ILO,JHI)) ) THEN
                     VAL = (DX-DY)*ARRAY(IHI,JLO) + DX1*ARRAY(ILO,JLO) +
     &                  DY*ARRAY(IHI,JHI)
                  ELSE
                     VAL = (DX1-DY)*ARRAY(ILO,JLO) + DX*ARRAY(IHI,JLO) +
     &                  DY*ARRAY(ILO,JHI)
                  END IF
                  X = X + INC
               IF( VAL .LT. ZMIN ) VAL = ZMIN
               IF( VAL .GT. ZMAX ) VAL = ZMAX
               IV = NINT((VAL-ZMIN) * RANGE)
               IF( POS ) IV = (LEVELS-1) - IV
               IC = IC + 1
               C(IC:IC) = CHAR( IV + MINIDX )
               IF( IC .GE. CMAX-1 ) THEN
                  WRITE( LUN, 200 ) C(1:IC)
                  IC = 1
                  C(1:1) = ' '
               END IF
    3          CONTINUE
               IF( IC .GT. 0 ) WRITE(LUN, 300) C(1:IC)
C
               Y = Y + INC
    4       CONTINUE
         END IF
C
C --- Close off the file.
C
    5    CONTINUE
         WRITE( LUN, 400 )
         CLOSE( UNIT=LUN )
      ELSE
         CALL ALGERR( RUTNAM, 'Cannot open specified output file:',
     &    FN )
      END IF
C
C
      RETURN
  100 FORMAT(' \hbox{\vbox{\halftone\offinterlineskip ',
     & '% machine-generated by TEXPIC.'/
     &  ' \def\BHT{\hbox\bgroup\ignorespaces}'/
     &  ' \catcode`\^=12 \catcode`\_=12 \catcode`\.=\active',
     &  ' \let.=\egroup'/ ' \catcode`\,=\active \let,=\BHT',
     &  ' \catcode`\/=0 \catcode`\\=12')
  200 FORMAT(1X,A,'%')
  300 FORMAT(1X,A,'.')
  400 FORMAT(' }}%')
      END
C --- -----------------------------------------------------------------
      SUBROUTINE TEXMAX( MV )
C --- -----------------------------------------------------------------
C
C --- TEXMAX version 0.0 was written by Alien in Fortran-77.
C
C --- This  routine sets the maximum number of pixels across a picture
C --- which TEXPIC will output to a file. Pictures  which  have  their
C --- first  dimension greater than MMAX are interpolated down to MMAX
C --- pixels.
C
C --- USAGE:  CALL TEXMAX( MMAX )
C
C --- PARAMETERS
C ---     MMAX  INTEGER  maximum number of pixels  to  be  plotted  by
C ---                    TEXPIC
C
C --- RESTRICTIONS
C --- none
C
C --- COMMONS
C --- /ALGTEX/
C
C --- SUBPROGRAMS INVOKED
C --- none
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
C
      INTEGER MV
C
      CHARACTER*(MAXICH) BUF
C
      INTEGER MMAX
C
      COMMON/ALGTEX/ MMAX
      SAVE /ALGTEX/
C
      IF( MV .GE. 2 ) THEN
         MMAX = MV
      ELSE
         WRITE( BUF, 100 ) MV
         CALL ALGERR( 'TEXMAX',
     &    'Too few pixels selected across page:'//BUF,
     &    'You must have two or more pixels across the page' )
      END IF
C
      RETURN
  100 FORMAT(I11)
      END
C --- ------------------------------------------------------------------
      SUBROUTINE ZRANGE( ZVMIN, ZVMAX )
C --- ------------------------------------------------------------------
C
C --- ZRANGE version 0.0 was written by Alien in Fortran-77.
C
C --- This  routine  fixes  the  range  of  the  Z-axis for subsequent
C --- graphical plots.
C
C --- USAGE:  CALL ZRANGE( ZMIN, ZMAX )
C
C --- PARAMETERS
C ---     ZMIN  REAL  minimum value to appear on the Z-axis
C ---     ZMAX  REAL  maximum value to appear on the Z-axis
C
C --- RESTRICTIONS
C --- ZMIN must be smaller than ZMAX.
C --- Note that the range of values actually produced on graphs may be
C --- slightly greater than those specified.
C
C --- SUBPROGRAMS INVOKED
C --- ALGERR
C
C --- COMMONS
C --- ZFIX, ZMIN, ZMAX in /ALG/
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
C
      REAL ZVMIN, ZVMAX
C
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
C
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
C
      IF( ZVMIN .LT. ZVMAX ) THEN
         ZMIN = ZVMIN
         ZMAX = ZVMAX
         ZFIX = .TRUE.
      ELSE
         CALL ALGERR('ZRANGE','Zmin was not smaller than Zmax.',' ')
      END IF
C
      RETURN
      END
C --- ------------------------------------------------------------------
      SUBROUTINE ZAUTO
C --- ------------------------------------------------------------------
C
C --- ZAUTO version 0.0 was written by Alien in Fortran-77.
C
C --- This routine causes  the  Z-axis  of  subsequent  graphical plots
C --- to be scaled according to the data being plotted.
C
C --- USAGE:  CALL ZAUTO
C
C --- PARAMETERS
C --- none
C
C --- RESTRICTIONS
C --- none
C
C --- SUBPROGRAMS INVOKED
C --- none
C
C --- COMMONS
C --- ZFIX in /ALG/
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
C
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
C
      ZFIX = .FALSE.
C
      RETURN
      END
C --- ------------------------------------------------------------------
      SUBROUTINE ZSAME
C --- ------------------------------------------------------------------
C ---                         ZSAME version 0.0
C
C ---  Written by Alien in Fortran-77. 
C
C ---  This  routine  causes  the  range  of  the  Z-axis on subsequent
C ---  graphical plots to be the same as those  used  on  the  previous
C ---  invocation. 
C
C ---  USAGE: CALL ZSAME 
C
C ---  PARAMETERS 
C ---  none 
C
C ---  RESTRICTIONS 
C ---  none 
C
C ---  SUBPROGRAMS INVOKED 
C ---  |ALGERR| 
C
C ---  COMMONS 
C ---  |ZFIX|, |ZMIN|, |ZMAX| in |/ALG/| 
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
C
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
C
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
C
      IF( ZMIN .LT. ZMAX ) THEN
         ZFIX = .TRUE.
      ELSE
         CALL ALGERR('ZSAME','Zmin was not smaller than Zmax.',
     &    'Call ignored.' )
      END IF
C
      RETURN
      END
C --- -----------------------------------------------------------------
      SUBROUTINE DONEG
C --- -----------------------------------------------------------------
C
C --- DONEG version 0.0 was written by Alien in Fortran-77.
C
C --- This routine causes subsequent grey-level pictures to  be  drawn
C --- with negative contrast.
C
C --- USAGE:  CALL DONEG
C
C --- PARAMETERS
C --- none
C
C --- RESTRICTIONS
C --- none
C
C --- SUBPROGRAMS INVOKED
C --- none
C
C --- COMMONS
C --- NEG in /ALG/
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
C
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
C
C
      NEG = .TRUE.
C
      RETURN
      END
C --- -----------------------------------------------------------------
      SUBROUTINE DOPOS
C --- -----------------------------------------------------------------
C
C --- DOPOS version 0.0 was written by Alien in Fortran-77.
C
C --- This routine causes subsequent grey-level pictures to  be  drawn
C --- with negative contrast.
C
C --- USAGE:  CALL DOPOS
C
C --- PARAMETERS
C --- none
C
C --- RESTRICTIONS
C --- none
C
C --- SUBPROGRAMS INVOKED
C --- none
C
C --- COMMONS
C --- NEG in /ALG/
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
C
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
C
C
      NEG = .FALSE.
C
      RETURN
      END
C --- ------------------------------------------------------------------
      SUBROUTINE MINMAX( ARRAY, M, N, LOWEST, HIEST )
C --- ------------------------------------------------------------------
C
C --- MINMAX version 0.0 was written by Alien in Fortran-77.
C
C --- This  routine  determines  the smallest and largest values of an
C --- array. If an interrupt is detected during the estimation of  the
C --- limits of the data, the currently-detected limits are returned.
C
C --- USAGE: CALL MINMAX( ARRAY, M, N, MIN, MAX )
C
C --- PARAMETERS
C ---     ARRAY  REAL     array of which the limits are to be
C ---                     determined
C ---     M      INTEGER  first dimension of ARRAY
C ---     N      INTEGER  second dimension of ARRAY
C ---     MIN    REAL     minimum value found in ARRAY (returned)
C ---     MAX    REAL     maximum value found in ARRAY (returned)
C
C --- RESTRICTIONS
C --- If  interrupts are to be detected, interrupt detection must have
C --- been enabled  by  the  calling  program---see  ABANDN  for  more
C --- details.
C
C --- SUBPROGRAMS INVOKED
C --- ABANDN
C
C --- COMMONS
C --- none
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
C
      INTEGER M, N
      REAL ARRAY(M*N), LOWEST, HIEST
C
      INTEGER I, J, JM
      REAL VAL, LO, HI
      INTEGER*2 CHAN
      LOGICAL ABFLAG
C
      COMMON /ALG_ABANDN/ ABFLAG, CHAN
      SAVE /ALG_ABANDN/
      EXTERNAL ALGINI
C
C --- We have declared ARRAY as a 1-D array, but will still access
C --- it via two DO-loops. This is so that ABANDN is only invoked once
C --- per "row" of ARRAY.
C
      LO = ARRAY(1)
      HI = LO
C
      DO 1 J = 1, N
         JM = (J-1) * M
      IF( ABFLAG ) GO TO 2
         DO 1 I = 1, M
            VAL = ARRAY(I+JM)
            IF( VAL .LT. LO ) LO = VAL
            IF( VAL .GT. HI ) HI = VAL
    1 CONTINUE
C
    2 CONTINUE
      LOWEST = LO
      HIEST = HI
C
      RETURN
      END
C --- ------------------------------------------------------------------
      SUBROUTINE ALGERR( NAME, MESS, EXTRA )
C --- ------------------------------------------------------------------
C
C --- ALGERR version 0.0 was written by Alien in Fortran-77.
C
C --- This routine reports errors generated by other routines. NAME is
C --- the  name  of the invoking routine while MESS and EXTRA form the
C --- message to be reported to the user. MESS  is  the  text  of  the
C --- message.  EXTRA,  if non-blank, contains extra information about
C --- the error; leading and trailing blanks are  removed  from  EXTRA
C --- before  it is output. The current version of this routine simply
C --- outputs the message text on the error output channel  --  future
C --- versions will be more sophisticated.
C
C --- USAGE: CALL ALGERR( NAME, MESS, EXTRA )
C
C --- PARAMETERS
C ---     NAME  CHARACTER*(*)  name of the invoking routine
C ---     MESS  CHARACTER*(*)  message to be output
C ---     EXTRA CHARACTER*(*)  additional text for the message
C
C --- RESTRICTIONS
C --- The  total  length  of  the message must be less than the output
C --- line length (usually 132 characters)  --  this  means  that  the
C --- lengths of NAME and MESS, when added together, must come to less
C --- then 110 characters.
C
C --- SUBPROGRAMS INVOKED
C --- none
C
C --- COMMONS
C --- /ALG/
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      CHARACTER*1 BLANK
      PARAMETER( BLANK=' ' )
C
      CHARACTER*(*) NAME, MESS, EXTRA
C
      INTEGER NEXTRA, FC, LC, NMESS, LM
C
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
C
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
      EXTERNAL ALGINI
C
      NMESS = LEN( MESS )
      DO 1 LM = NMESS, 1, -1
         IF( MESS(LM:LM) .NE. BLANK ) GO TO 2
    1 CONTINUE
      LM = 1
    2 CONTINUE
C
      IF( EXTRA .EQ. BLANK ) THEN
         WRITE( ELUN, 100 ) NAME, MESS(1:LM)
      ELSE
         NEXTRA = LEN( EXTRA )
         DO 3 FC = 1, NEXTRA
            IF( EXTRA(FC:FC) .NE. BLANK ) GO TO 4
    3    CONTINUE
    4    CONTINUE
         DO 5 LC = NEXTRA, FC, -1
            IF( EXTRA(LC:LC) .NE. BLANK ) GO TO 6
    5    CONTINUE
    6    CONTINUE
         WRITE( ELUN, 100 ) NAME, MESS(1:LM)
         WRITE( ELUN, 101 ) EXTRA(FC:LC)
      END IF
C
      RETURN
  100 FORMAT(1X,A,': error -- ',A)
  101 FORMAT(10X,A)
      END
C --- ------------------------------------------------------------------
      LOGICAL FUNCTION ABANDN( OP )
C --- ------------------------------------------------------------------
C
C --- ABANDN version 0.1 was written by Alien in Fortran-77.
C
C --- This  LOGICAL  function  is  used to detect whether the user has
C --- tried to interrupt execution.
C
C ---    The method of specifying an interrupt varies from  system  to
C --- system,   but   is  typically  by  typing  a  control  character
C --- (control-C on the VAX). ABANDN is used  with  OP  =  0  to  TEST
C --- whether the user has signalled an interrupt -- the value TRUE is
C --- returned  as  the  value  of  the  function if this is the case.
C --- ABANDN is used with OP = 1 to SET or CLEAR the  interrupt  trap;
C --- this  must  be  done  by  the  calling program. Note that ABANDN
C --- returns the value TRUE if an error  occurred  while  setting  or
C --- clearing the interrupt trap.
C
C --- USAGE:   <logical variable> = ABANDN( OP )
C
C --- PARAMETERS
C ---     OP  INTEGER  operation 0 ==> test, 1 ==> set
C
C --- RESTRICTIONS
C --- Interrupts  will  not  be trapped before the first invocation of
C --- ABANDN(1).
C --- After  the  user  has   signalled   an   interrupt,   subsequent
C --- invocations  of  ABANDN(0)  will  return TRUE until ABANDN(1) is
C --- used to clear it.
C --- If the user generates interrupts very quickly (for  example,  by
C --- letting  the  ^C  auto-repeat), they may be delivered so quickly
C --- that ABANDN does not manage to reset its trap in time;  in  this
C --- case, it will actually interrupt program execution.
C --- This version of ABANDN requires VAX/VMS 3.0 or later.
C
C --- SUBPROGRAMS INVOKED
C --- ALG_ABANDN_AST (condition handler)
C
C --- COMMONS
C --- /ALG_ABANDN/
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
      INCLUDE '($IODEF)'
C
      INTEGER ABSET, ABTEST
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
      PARAMETER( ABSET=1, ABTEST=0 )
C
      INTEGER OP
C
      CHARACTER*(MAXICH) CODE
      INTEGER F
      INTEGER IOS, VAL, SYS$GETDVI, SYS$ASSIGN, SYS$QIOW, SYS$DASSGN
      INTEGER DVIBLK(4)/ '40004'X, 0, 0, 0/
      INTEGER*2 IOSB(4)
      LOGICAL FIRST/.TRUE./
      EXTERNAL ALG_ABANDN_AST
      INTEGER*2 CHAN
      LOGICAL ABFLAG
C
      COMMON /ALG_ABANDN/ ABFLAG, CHAN
      SAVE /ALG_ABANDN/
C
C --- Branch according to the value of OP.
C
      IF( OP .EQ. ABTEST ) THEN
         ABANDN = ABFLAG
      ELSE IF( OP .EQ. ABSET ) THEN
         ABANDN = .FALSE.
         IF( FIRST ) THEN
C
C --- Check that we're using a terminal.
C
            DVIBLK(2) = %LOC( VAL )
            IOS = SYS$GETDVI( ,, 'TT', DVIBLK, ,,, )
            IF( .NOT. IOS ) CALL EXIT( IOS )
            IF( VAL .EQ. '42'X ) THEN
C
C --- If we're using a terminal, assign a channel to the device and set
C --- the trap; failure from any of the system services is taken as a
C --- fatal error. We will only return a failure code to the user if
C --- IOSB(1) indicates an error.
C
               IOS = SYS$ASSIGN( 'TT', CHAN,, )
               IF( .NOT. IOS ) CALL EXIT( IOS )
               IOS = SYS$QIOW(, %VAL(CHAN), %VAL(IO$_SETMODE.OR.IO$M_CTR
     &LCAST),
     &            IOSB, ,,   ALG_ABANDN_AST, ,,,, )
               IF( .NOT. IOS ) CALL EXIT( IOS )
               IF( IOSB(1) ) THEN
                  FIRST = .FALSE.
               ELSE
C
C --- We didn't succeed in setting the interrupt trap, heaven knows why!
C --- Set the function to return an error code, then close the channel
C --- we
C --- have so carefully opened.
C
                  ABANDN = .TRUE.
                  IOS = SYS$DASSGN( %VAL(CHAN) )
                  IF( .NOT. IOS ) CALL EXIT( IOS )
               END IF
            END IF
         END IF
         ABFLAG = .FALSE.
      ELSE
         STOP 'ABANDN: Illegal argument value.'
      END IF
C
      RETURN
  100 FORMAT(I11)
      END
C --- ------------------------------------------------------------------
      SUBROUTINE ALG_ABANDN_AST
C --- ------------------------------------------------------------------
C
C --- This routine is the condition handler which is used in the
C --- implementation of ABANDN for VAX/VMS. It is called by the system
C --- when the user types ^C at his terminal; its main purpose is to set
C --- ABFLAG in common /ALG_ABANDN/, to be tested by ABANDN(ABTEST).
C --- However, because VMS ^C condition handlers are one-shot affairs,
C --- we must also re-impose the trap. This is done by invoking the
C --- simple routine ALG_ABANDN_RESET_TRAP.
C
      INTEGER*2 CHAN
      LOGICAL ABFLAG
C
      COMMON /ALG_ABANDN/ ABFLAG, CHAN
      SAVE /ALG_ABANDN/
C
C --- Set ABFLAG.
C
      ABFLAG = .TRUE.
C
C --- And reset the trap.
C
      CALL ALG_ABANDN_RESET_TRAP
C
      RETURN
      END
C --- ------------------------------------------------------------------
      SUBROUTINE ALG_ABANDN_RESET_TRAP
C --- ------------------------------------------------------------------
C
C --- This routine resets the ^C trap for ALG_ABANDN_AST, because the
C --- Fortran compiler will not allow ALG_ABANDN_AST to be used in the
C --- $QIOW call inside its own code.
C
      INCLUDE '($IODEF)'
      INTEGER*2 IOSB(4)
      INTEGER IOS, SYS$QIOW
      EXTERNAL ALG_ABANDN_AST
C
      INTEGER*2 CHAN
      LOGICAL ABFLAG
C
      COMMON /ALG_ABANDN/ ABFLAG, CHAN
      SAVE /ALG_ABANDN/
C
      IOS = SYS$QIOW(, %VAL(CHAN), %VAL(IO$_SETMODE.OR.IO$M_CTRLCAST),
     &   IOSB, ,,   ALG_ABANDN_AST, ,,,, )
      IF( .NOT. IOS ) CALL EXIT( IOS )
C
      RETURN
      END
C --- ------------------------------------------------------------------
           B L O C K    D A T A    A L G I N I
C --- ------------------------------------------------------------------
C
C --- Copyright (C) Alien 1987 -- All rights reserved.
C
      IMPLICIT NONE
C
C --- The following definitions are used to allow expressions to be
C --- typed for the initial values of variables.
C
      INTEGER GRFGRF, GRFHIS, GRFPOL
      INTEGER MAXICH, MAXRCH
      INTEGER MINLUN, MAXLUN
      INTEGER MAXOUT, MAXARR, MAXAFT, MAXCH
      INTEGER MAXSTV, NOUSEM
      REAL PI
      INTEGER ORDRED, RNDOM, STRAIT
C
      PARAMETER( GRFGRF=0, GRFHIS=1, GRFPOL=2 )
      PARAMETER( MAXICH=11, MAXRCH=11 )
      PARAMETER( MINLUN=1, MAXLUN=99 )
      PARAMETER( MAXOUT=133, MAXARR=512, MAXAFT=(9*MAXARR)/4+1 )
      PARAMETER( MAXCH=16 )
      PARAMETER( MAXSTV=3, NOUSEM=-1 )
      PARAMETER( PI=3.1415926535897932384626433 )
      PARAMETER( ORDRED=1, RNDOM=2, STRAIT=0 )
      INTEGER MOUT1, MSTAT
      PARAMETER (MOUT1=MAXOUT-1, MSTAT=MAXSTV+1)
C
C --- Include all the common variables and blocks.
C
      INTEGER GLUN, PLUN, ELUN, POLHIS
      LOGICAL XFIX, YFIX, ZFIX, RFIX, TFIX, LOGX, LOGY, LOGZ, LOGR
      LOGICAL VRBOSE, NEG, MIDORG, FTNRML
      REAL XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX, TMIN, TMAX,
     & FTFWD, FTREV
      REAL TOL
      INTEGER*2 CHAN
      LOGICAL ABFLAG
      CHARACTER*1 BLANK, VMARK, HMARK, TICK
      CHARACTER*(MAXOUT) LPBUF
      INTEGER LPHT, LPWID
      LOGICAL FF
      REAL ASPECT
      LOGICAL USEM, EXACT, KNOWEM(0:MAXSTV)
      REAL MINS(0:MAXSTV), MAXS(0:MAXSTV), MEANS(0:MAXSTV),
     &   SDS(0:MAXSTV)
      INTEGER MMAX
      INTEGER METHOD
C
      COMMON /ALG/ XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, RMIN, RMAX,
     & TMIN, TMAX, TOL, FTFWD, FTREV, XFIX, YFIX, ZFIX, RFIX, TFIX,
     & LOGX, LOGY, LOGZ, LOGR, NEG, GLUN, PLUN, ELUN, POLHIS, MIDORG,
     & FTNRML, VRBOSE
      SAVE /ALG/
      COMMON /ALG_ABANDN/ ABFLAG, CHAN
      SAVE /ALG_ABANDN/
      COMMON /LPC/ LPBUF, BLANK, VMARK, HMARK, TICK
      COMMON /LPN/ ASPECT, LPHT, LPWID, FF
      SAVE /LPC/, /LPN/
      COMMON /STAT/ MINS, MAXS, MEANS, SDS, USEM, KNOWEM, EXACT
      SAVE /STAT/
      COMMON/ALGTEX/ MMAX
      SAVE /ALGTEX/
      COMMON /V80/ METHOD
C
C --- / A L G /
C
      DATA LOGX/.FALSE./, LOGY/.FALSE./, LOGZ/.FALSE./, LOGR/.FALSE./
      DATA XMIN/0.0/, XMAX/0.0/, YMIN/0.0/, YMAX/0.0/, ZMIN/0.0/,
     & ZMAX/0.0/, RMIN/0.0/, RMAX/0.0/, TMIN/0.0/, TMAX/0.0/
      DATA XFIX/.FALSE./, YFIX/.FALSE./, ZFIX/.FALSE./
      DATA RFIX/.FALSE./, TFIX/.FALSE./
      DATA VRBOSE/.FALSE./, NEG/.FALSE./
      DATA MIDORG/.TRUE./, FTNRML/.FALSE./
      DATA GLUN/6/, PLUN/6/, ELUN/6/, POLHIS/GRFGRF/
      DATA TOL/1.0E-8/
      DATA FTFWD/0.0/, FTREV/0.0/
C
C --- / A B A N D N /
C
      DATA ABFLAG/.FALSE./
C
C --- / A L G T E X /
C
      DATA MMAX/256/
C
C --- / L P C /
C
      DATA BLANK/' '/, VMARK/'|'/, HMARK/'-'/, TICK/'+'/
C
C --- / L P N /
C
      DATA FF/.TRUE./
      DATA LPHT/62/, LPWID/MOUT1/
      DATA ASPECT/ 0.604 /
C
C --- / S T A T /
C
      DATA USEM/NOUSEM/, KNOWEM/MSTAT*.FALSE./, EXACT/.FALSE./
      DATA MEANS/MSTAT*0.0/, SDS/MSTAT*0.0/
      DATA MINS/MSTAT*0.0/, MAXS/MSTAT*0.0/
C
C --- / V 8 0 /
C
      DATA METHOD/STRAIT/
C
      END
