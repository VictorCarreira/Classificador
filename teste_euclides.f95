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

ALLOCATE(lito1(5,5),lito2(5,5))

lito1=0d0
lito2=0d0

DO i=1,5
 DO j=1,5
  lito1(i,j)=1.0
 END DO
END DO

DO i=1,5
 DO j=1,5
  lito1(i,j)=1.0
 END DO
END DO


PRINT*, lito1
PRINT*,lito2



CALL euclideana(lito1,lito2,eucli)
!21 FORMAT(4(D16.16, 4X))
!DO i=1,SIZE(eucli)
!PRINT*, eucli(i), i
!ENDDO






!*******************************************************************************************!


CONTAINS

SUBROUTINE euclideana(lito1,lito2,eucli)

 IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

 REAL(KIND=DP), DIMENSION(:,:), INTENT(IN)::lito1, lito2
 REAL(KIND=DP), DIMENSION(:), INTENT(INOUT):: eucli

 INTEGER(KIND=SP):: i, nt, nc, n, k

  nt=SIZE(lito1,1) !nt, número de linhas do arquivo de treinamento
  nc=SIZE(lito2,1) !nc, número de linhas do arquivo de classificação

  IF (nt>nc)THEN
    n=nt
   ELSE IF (nt<nc) THEN
    n=nc
    ELSE
    n=nt
  END IF


 ! Precisa calcular o centróide
 !o centróide é a média aritimética das propriedades físicas
 DO i=1,n
  DO k=1,SIZE(lito1,2)
  eucli(i)= SQRT((lito1(i,k)**2-lito2(i,k)**2))
  ENDDO
ENDDO

PRINT*,n,nt,nc,SIZE(lito1,2)


END SUBROUTINE euclideana

END PROGRAM teste_euclides
