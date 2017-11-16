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
	
!INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=4)
REAL, PARAMETER::DP = SELECTED_REAL_KIND(p=8, r=8)   
REAL(KIND=DP):: a1, a2, a3, a4 
!INTEGER:: i, j, k
CHARACTER(LEN=15):: cab(7), lito(7)

TYPE litologia
 INTEGER:: COMP
 CHARACTER(LEN= 20) :: PALAVRA
END TYPE litologia

TYPE propriedade
 INTEGER:: COMP
 CHARACTER(LEN= 20) :: PALAVRA
END TYPE propriedade

TYPE(litologia) :: folhelho, dolomita, diabasio, conglomerado, embasamento 
TYPE(propriedade):: densidade, gama, resistividade, velocidade  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!CRIANDO OS ARQUIVOS E OS FORMATOS  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

open(1,file='dados_sint_t1.txt') ! entrada do programa 						
open(2,file='saida_class.txt')   ! arquivo de saída do programa
OPEN(3,file='dados_sint_c1.txt') ! segunda entrada 		
OPEN(4,file='folhelho.txt')      ! subset de dados 
		

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
	read(1,15) cab    ! cabeçalho
!	write(6,15) cab
	read(1,15) cab    ! linha em branco abaixo do cabeçalho
!	write(6,15) cab

	ij=1
	do while (.true.)
	read(1,*,end=6) branco,a1,a2,a3,a4,a5,a6
	ij=ij+1
	end do
6 continue
	close(1)

	nt=ij-1
	write(6,*) "n de dados de treinamento",nt


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



	allocate (tr(1:nt,4),cl(1:nt),prof(1:nt),hip(1:nt,5,9))

	open(1,file='dados_sint_T1.txt')

	read(1,15) cab    ! cabe�alho
!	write(6,15) cab
	read(1,15) cab    ! linha em branco abaixo do cabe�alho
!	write(6,15) cab
		
	do i=1,nt
	read(1,*) branco,cl(i),prof(i),tr(i,1),tr(i,2),tr(i,3),tr(i,4)
	end do
	close(1)


	do i=1,nt
	
	do j=1,5
	if(cl(i) == dfloat(j))then
	ic1(j)=ic1(j)+1
	hip(ic1(j),1,j)=prof(i)
	hip(ic1(j),2,j)=tr(ic1(j),1)
	hip(ic1(j),3,j)=tr(ic1(j),2)
	hip(ic1(j),4,j)=tr(ic1(j),3)
	hip(ic1(j),5,j)=tr(ic1(j),4)
	end if
	end do 


	i1=5
	do j=451,454
	i1=i1+1
	if(cl(i) == dfloat(j))then
	ic2(j)=ic2(j)+1
	hip(ic2(j),1,i1)=prof(i)
	hip(ic2(j),2,i1)=tr(ic2(j),1)
	hip(ic2(j),3,i1)=tr(ic2(j),2)
	hip(ic2(j),4,i1)=tr(ic2(j),3)
	hip(ic2(j),5,i1)=tr(ic2(j),4)
	end if
	end do 

	end do


	write(6,*) 'n de folhelhos=',ic1(1)
	write(6,*) 'n de dolomitas=',ic1(2)

	i25=ic1(1)
	do i=1, i25
	write(6,*) 'densidade dos folhelhos=',hip(i,2,1)
	end do


	i25=ic1(2)
	do i=1, i25
	write(6,*) 'densidade das dolomitas=',hip(i,2,2)
	end do


END PROGRAM preclassificador