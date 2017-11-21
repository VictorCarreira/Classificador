PROGRAM preclassificador
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação da subrotina do cálculo de distância por Mahalanobis                 !
  !Orientador: Cosme Ferreira da Ponte Neto                                     !
  !Aluno: Victor Ribeiro Carreira                                               !
  !Este programa visa criar agrupamento de dados de litologias de poços         !
  !Cálculo de distâncias em um                                                  !
  !Subrotina Mahalanobis                                                        !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
 IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!! DECLARAÇÃO DAS VARIÁVEIS GLOBAIS !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
REAL, PARAMETER::DP = SELECTED_REAL_KIND(p=12, r=12, radix=12)

INTEGER(KIND=SP):: i, j, ij, nt, i1, i25
INTEGER(KIND=SP), ALLOCATABLE, DIMENSION(:):: ic1, ic2 

REAL(KIND=SP):: a1, a2, a3, a4, a5, a6, branco
REAL(KIND=SP), ALLOCATABLE, DIMENSION(:):: prof, cl
REAL(KIND=SP), ALLOCATABLE, DIMENSION(:,:)::tr
REAL(KIND=SP), ALLOCATABLE, DIMENSION(:,:,:)::hip

CHARACTER(LEN=15):: cab(80), lito(80)

TYPE litologia
 INTEGER:: COMP
 CHARACTER(LEN= 20) :: PALAVRA
END TYPE litologia

TYPE propriedade
 INTEGER:: COMP
 CHARACTER(LEN= 20) :: PALAVRA
END TYPE propriedade

!TYPE(litologia) :: folhelho, dolomita, diabasio, conglomerado, embasamento 
!TYPE(propriedade):: densidade, gama, resistividade, velocidade  


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!CRIANDO OS ARQUIVOS E OS FORMATOS  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

OPEN(1,file='dados_sint_t1.txt') ! entrada do programa 						
!OPEN(2,file='saida_class.txt')   ! arquivo de saída do programa
OPEN(3,file='dados_sint_c1.txt') ! segunda entrada 		
!OPEN(4,file='folhelho.txt')      ! subset de dados 
		

! Aqui são os formatos criados para edição dos arquivos de entrada e de saída
10 FORMAT(A8, 8x, O1, 11x, O1, 3x, 4(ES1.2E2, 3x))

11 FORMAT(4(ES12.4E3,2x))
12 FORMAT(I3,2x,3(f6.2,2x))
13 FORMAT(I2,3x,I10,2x,4(ES9.2E2,2x))
14 FORMAT(A12,2x,I3,2x,I10,2x,4(ES9.2E2,2x))
15 FORMAT(A9,5x,A6,6x,A4,2x,A4,7x,A4,7x,A3,8x,A3)
16 FORMAT(A11,8(ES9.2E3))

		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!ARMAZENANDO AS VARIÁVEIS DE ENTRADA !!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!Leitura do arquivo de entrada
 READ(1,15) cab    ! cabeçalho
!	write(6,15) cab
 READ(1,15) cab    ! linha em branco abaixo do cabeçalho
!	write(6,15) cab

 ij=1
 
 DO WHILE(.TRUE.)
  READ(1,*,END=6) branco,a1,a2,a3,a4,a5,a6
  ij=ij+1
 END DO
 6 CONTINUE  

CLOSE(1)

 nt=ij-1
 WRITE(6,*) "n de dados de treinamento",nt


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



 ALLOCATE(tr(1:nt,4),cl(1:nt),prof(1:nt),hip(1:nt,5,9))

 OPEN(1,file='dados_sint_t1.txt')

 READ(1,15) cab    ! cabeçalho
!	write(6,15) cab
 READ(1,15) cab    ! linha em branco abaixo do cabeçalho
!	write(6,15) cab
		
 DO i=1,nt
  READ(1,*) branco,cl(i),prof(i),tr(i,1),tr(i,2),tr(i,3),tr(i,4)
 END DO 
 
 CLOSE(1)

!Zerando variáveis
 hip=0d0
 ic1=0
 ic2=0

 DO i=1,nt
  DO j=1,5
   IF(cl(i) == dfloat(j))THEN
    ic1(j)=ic1(j)+1
     hip(ic1(j),1,j)=prof(i)
     hip(ic1(j),2,j)=tr(ic1(j),1)
     hip(ic1(j),3,j)=tr(ic1(j),2)
     hip(ic1(j),4,j)=tr(ic1(j),3)
     hip(ic1(j),5,j)=tr(ic1(j),4)
   END IF
  END DO  
  i1=5
  DO j=451,454
   i1=i1+1
    IF(cl(i) == dfloat(j))THEN 
     ic2(j)=ic2(j)+1
     hip(ic2(j),1,i1)=prof(i)
     hip(ic2(j),2,i1)=tr(ic2(j),1)
     hip(ic2(j),3,i1)=tr(ic2(j),2)
     hip(ic2(j),4,i1)=tr(ic2(j),3)
     hip(ic2(j),5,i1)=tr(ic2(j),4)
    END IF
  END DO  
 END DO


 WRITE(6,*) 'n de folhelhos=',ic1(1)
 WRITE(6,*) 'n de dolomitas=',ic1(2)

 i25=ic1(1)
 DO i=1, i25
  WRITE(6,*) 'densidade dos folhelhos=',hip(i,2,1)
 END DO

 i25=ic1(2)
 DO i=1, i25
  WRITE(6,*) 'densidade das dolomitas=',hip(i,2,2)
 END DO 

END PROGRAM preclassificador