PROGRAM teste_euclides

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação da subrotina do cálculo de distância euclideana                     !
  !Orientador: Cosme Ferreira da Ponte Neto                                    !
  !Aluno: Victor Ribeiro Carreira                                              !
  !Este programa validar a subrotina euclideana                                !
  !Categoria: classificador                                                    !
  !Subrotina euclideana                                                        !
  !Para usar compilação com flags utilize:                                     !
  !gfortran -fbounds-check -fbacktrace -Wall -Wextra -pedantic                 !
  !"pasta/subpasta/nomedopragrama.f95" -o nomedoexecutável                     !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

IMPLICIT NONE

INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

INTEGER(KIND=SP):: i,j
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: eucli
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:)::lito1, lito2

21 FORMAT(5(F4.2,2x))

ALLOCATE(lito1(5,5),lito2(5,5))

lito1=0d0
lito2=0d0

DO i=1,5
 DO j=1,5
   IF(i>j)THEN
     lito1(i,j)=3.42
   ELSE IF(i<j)THEN
     lito1(i,j)=1.72
   ELSE
     lito1(i,j)=8.98
   ENDIF
 END DO
END DO

DO i=1,5
 DO j=1,5
   IF(i>j)THEN
     lito2(i,j)=7.72
   ELSE IF(i<j)THEN
     lito2(i,j)=6.98
   ELSE
     lito2(i,j)=0.25
   ENDIF
 END DO
END DO

PRINT*,'---------------------'
PRINT*,'Matriz lito1'
PRINT*,'---------------------'
WRITE(*,21) lito1
PRINT*,'---------------------'
PRINT*,'Matriz lito2'
PRINT*,'---------------------'
WRITE(*,21) lito2
PRINT*,'---------------------'



CALL euclideana(lito1,lito2,eucli)
PRINT*, eucli





!*******************************************************************************************!


CONTAINS

SUBROUTINE euclideana(lito1,lito2,eucli)

 IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

 REAL(KIND=DP), DIMENSION(:,:), INTENT(IN)::lito1, lito2
 REAL(KIND=DP), DIMENSION(:), INTENT(INOUT):: eucli
 REAL(KIND=DP)::media1, media2

 INTEGER(KIND=SP):: nt, nc, n!, k

  nt=SIZE(lito1,1) !nt, número de linhas do arquivo de treinamento
  nc=SIZE(lito2,1) !nc, número de linhas do arquivo de classificação


  media1=0d0
  media2=0d0
  media1=SUM(lito1(1,:))/SIZE(lito1(1,:))
  media2=SUM(lito2(1,:))/SIZE(lito2(1,:))
  PRINT*,'Media da coluna 1 lito1=',media1
  PRINT*,'Media da coluna 1 lito2=',media2

  IF (nt>nc)THEN
    n=nt
   ELSE IF (nt<nc) THEN
    n=nc
    ELSE
    n=nt
  END IF

 ! UBOUND(ARRAY [, DIM [, KIND]])


 ! Precisa calcular o centróide
 !o centróide é a média aritimética das propriedades físicas
 !eucli=SQRT(X-Xmeani)^1/2

  !DO k=1,SIZE(lito1(1,:))
  eucli= SQRT((lito1(1,1)-media1)**2+(lito2(1,1)-media2)**2)
  !ENDDO




END SUBROUTINE euclideana


END PROGRAM teste_euclides
