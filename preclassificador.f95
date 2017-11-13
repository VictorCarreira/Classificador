PROGRAM preclassificador
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação da subrotina do cálculo de distância por Mahalanobis                 !
  !Orientador: Cosme Ferreira da Ponte Neto                                     !
  !Aluno: Victor Ribeiro Carreira                                               !
  !Este programa visa criar agrupamento de dados de litologias de poços         !
  !Cálculo de distâncias em um                                                       !
  !Subrotina Mahalanobis                                                        !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
 IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!! DECLARAÇÃO DAS VARIÁVEIS GLOBAIS !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
!INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
REAL, PARAMETER::DP = SELECTED_REAL_KIND(p=8, r=8)   
!REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:,:):: folhelho 
!INTEGER:: i, j, k
CHARACTER(LEN=15):: cab(7), lito(7)

TYPE litologia
INTEGER:: COMP
CHARACTER(LEN= 20) :: PALAVRA
END TYPE litologia

TYPE(litologia) :: folhelho


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!LENDO OS ARQUIVOS E CRIANDO OS FORMATOS !!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


OPEN(1,file='dados_sint_c1.txt')						


11 FORMAT(A15)
		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!ARMAZENANDO AS VARIÁVEIS DE ENTRADA !!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

READ(1,*) cab
!READ(*,11) lito
READ(*,11), folhelho

!PRINT*, cab
!PRINT 11,lito
PRINT*,folhelho



END PROGRAM preclassificador