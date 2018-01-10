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
REAL(KIND=DP)::eucli
REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:)::lito1, lito2

21 FORMAT(15(F4.2,2x))

ALLOCATE(lito1(5,15),lito2(1,15))

lito1=0d0
lito2=0d0

DO i=1,5
 DO j=1,15
   IF(i>j)THEN
     lito1(i,j)=3.42
   ELSE IF(i<j)THEN
     lito1(i,j)=1.72
   ELSE
     lito1(i,j)=8.98
   ENDIF
   !lito1(i,j)=2.0
 END DO
END DO

DO i=1,1
 DO j=1,15
   IF(i>j)THEN
     lito2(i,j)=7.72
   ELSE IF(i<j)THEN
     lito2(i,j)=6.98
   ELSE
     lito2(i,j)=0.25
   ENDIF
   !lito2(i,j)=3.0
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
PRINT*,'Distância euclideana=', eucli





!*******************************************************************************************!


CONTAINS

SUBROUTINE euclideana(lito1,lito2,eucli)

 IMPLICIT NONE
 INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
 INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

 REAL(KIND=DP), DIMENSION(:,:), INTENT(IN)::lito1, lito2
 REAL(KIND=DP), INTENT(OUT):: eucli
 REAL(KIND=DP)::media1

 INTEGER(KIND=SP):: k

  eucli=0d0
  
  IF(SIZE(lito1(1,:)) /= SIZE(lito2(1,:)))THEN
    PRINT*,'WARNING! THE PROPERTIES NUMBER´S OF lito1 AND lito2 MUST BE THE SAME.'
    STOP
    RETURN
  END IF  
  
  DO k=1,SIZE(lito1(1,:))  ! Inicia o laço da primeira até a última propriedade que é dado pelo size de lito
   media1=0d0 !zera as variáveis 
   media1=SUM(lito1(:,k))/SIZE(lito1(:,k)) !calcula as médias para as k propriedades
  
      eucli= eucli + SQRT((lito2(1,k)-media1)**2) ! Cálculo da medida de semelhança de euclides 
  END DO ! Final do laço das k propriedades  

END SUBROUTINE euclideana


END PROGRAM teste_euclides
