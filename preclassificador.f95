PROGRAM preclassificador
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação da subrotina do cálculo de distância por Mahalanobis                 !
  !Orientador: Cosme Ferreira da Ponte Neto                                     !
  !Aluno: Victor Ribeiro Carreira                                               !
  !Este programa visa criar agrupamento de dados de litologias de poços         !
  !Cálculo de distâncias em um                                                  !
  !Subrotina Mahalanobis                                                        !
  !Para usar compilação com flags utiilze:                                      !
  !gfortran -fbounds-check -fbacktrace -Wall -Wextra -pedantic                  ! 
  !"pasta/subpasta/nomedopragrama.f95" -o nomedoexecutável                      !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

 

 IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!! DECLARAÇÃO DAS VARIÁVEIS GLOBAIS !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(10,100) 

   INTEGER(KIND=SP):: ij, nt
 !  INTEGER(KIND=SP), ALLOCATABLE, DIMENSION(:):: ic1, ic2 

  REAL(KIND=DP):: a1, a2, a3, a4, a5, a6
   
   REAL(KIND=SP):: inicial, final, custocomputacional
 !  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: prof, cl
 !  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:)::tr
 !  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:)::hip

 !CHARACTER(LEN=80):: cab(7)

 TYPE litologia
  !INTEGER:: COMP
  CHARACTER(LEN= 15) :: PALAVRA(7)
 END TYPE litologia

 TYPE propriedade
  !INTEGER:: COMP
  CHARACTER(LEN= 15) :: PALAVRA 
 END TYPE propriedade

 TYPE(litologia):: cabecalho, branco, rocha!, codigo, profundidade 
 !TYPE(propriedade):: densidade, gama, resistividade, velocidade
 

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!!!!!!!!!!!!!!!!CRIANDO OS ARQUIVOS E OS FORMATOS  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 CALL cpu_time(inicial)

 ! Aqui são os formatos criados para edição dos arquivos de entrada e de saída
 !10 FORMAT(A8, 8x, O1, 11x, O1, 3x, 4(ES1.2E2, 3x))
 !11 FORMAT(4(ES12.4E3,2x))
 !12 FORMAT(I3,2x,3(f6.2,2x))
 !13 FORMAT(I2,3x,I10,2x,4(ES9.2E2,2x))
 !14 FORMAT(A12,2x,I3,2x,I10,2x,4(ES9.2E2,2x))
  15 FORMAT(A9,5x,A6,6x,A4,2x,A4,7x,A4,7x,A3,8x,A3)
 !16 FORMAT(A11,8(ES9.2E3))
 !17 FORMAT(A30,2x,ES12.4E3)
 !18 FORMAT(2(f6.2,2x),2x,A11,2x,ES12.4E3)

  OPEN(1,file='dados_sint_T1.txt') ! entrada do programa 						
   
		



		
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!ARMAZENANDO AS VARIÁVEIS DE ENTRADA !!!!!!!!!!!!!!!!!!
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Leitura do arquivo de treinamento

   READ(1,15) cabecalho    ! leitura do cabeçalho
   WRITE(6,15) cabecalho
   READ(1,15) branco    ! linha em branco abaixo do cabeçalho
   WRITE(6,15) branco
   
    ij=1
     DO WHILE (.TRUE.)
     READ(1,*,end=6) rocha, a1, a2, a3, a4, a5, a6
     ij=ij+1
     END DO
     6 CONTINUE
  CLOSE(1)

   nt=ij-1
 ! 	write(6,*) "n de dados de treinamento",nt

 ! !!!!!!!!!!!!!
 ! !        Leitura do arquivo de dados a serem classificados

 ! 	read(2,15) cab    ! cabeçalho
 ! !	write(6,15) cab
 ! 	read(2,15) cab    ! linha em branco abaixo do cabeçalho
 ! !	write(6,15) cab

 ! 	ij=1
 ! 	do while (.true.)
 ! 	read(2,*,end=7) branco,a1,a2,a3,a4,a5,a6
 ! 	ij=ij+1
 ! 	end do
 ! 7 continue
 ! 	close(2)

 ! 	ntc=ij-1 
 ! 	write(6,*) "n de dados a serem classificados",ntc


 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



 ! 	allocate (tr(nt,4),cl(nt),prof(nt),hip(nt,5,9))


 ! 	hip=0d0
 ! 	ic1=0
 ! 	ic2=0

 ! 	open(1,file='dados_sint_T1.txt')

 ! 	read(1,15) cab    ! cabeçalho
 ! !	write(6,15) cab
 ! 	read(1,15) cab    ! linha em branco abaixo do cabeçalho
 ! !	write(6,15) cab
		
 ! 	do i=1,nt
 ! 	read(1,*) branco,cl(i),prof(i),tr(i,1),tr(i,2),tr(i,3),tr(i,4)
 ! 	end do
 ! 	close(1)



 ! 	do i=1,nt
	
 ! 	do j=1,5
 ! 	if(cl(i) == dfloat(j))then
 ! 	ic1(j)=ic1(j)+1
 ! 	hip(ic1(j),1,j)=prof(i)
 ! 	hip(ic1(j),2,j)=tr(i,1)
 ! 	hip(ic1(j),3,j)=tr(i,2)
 ! 	hip(ic1(j),4,j)=tr(i,3)
 ! 	hip(ic1(j),5,j)=tr(i,4)
 ! 	end if
 ! 	end do 




 ! 	nlito=450	
 ! 	do j=1,4
 ! 	nlito=nlito+1
 ! 	if(cl(i) == dfloat(nlito))then
 ! 	ic2(j)=ic2(j)+1
 ! 	hip(ic2(j),1,5+j)=prof(i)
 ! 	hip(ic2(j),2,5+j)=tr(i,1)
 ! 	hip(ic2(j),3,5+j)=tr(i,2)
 ! 	hip(ic2(j),4,5+j)=tr(i,3)
 ! 	hip(ic2(j),5,5+j)=tr(i,4)
 ! 	end if
	
 ! 	end do 






 ! !	print*, " até aqui, tudo bem"
 ! !	pause

 ! 	end do

 ! 	allocate (lito1(ic1(1),4),lito2(1,4))

 !  lito1=0d0
 !  lito2=0d0

 ! 	print*, 'ic1=',size(ic1,1)
 ! 	print*, 'ic2=',size(ic2,1)




 ! 	write(6,*) 'n de folhelhos=',ic1(1)
 ! 	write(6,*) 'n de dolomitas=',ic1(2)
 ! 	write(6,*) 'n de congl-emb1=',ic2(1)

 ! 	write(6,*) '========================'

 ! 	do i=1,ic1(1)
 ! 	write(6,*) 'densidade dos folhelhos=',hip(i,1,1),hip(i,2,1)
 ! 	end do

 ! 	write(6,*) '========================'

 ! 	do i=1, ic1(2)
 ! 	write(6,*) 'densidade das dolomitas=',hip(i,1,2),hip(i,2,2)
 ! 	end do

 ! 	write(6,*) '========================'
 ! 	do i=1,ic1(3)
 ! 	write(6,*) 'densidade dos diabasio=',hip(i,1,3),hip(i,2,3)
 ! 	end do


 ! 	write(6,*) '========================'
 ! 	do i=1,ic1(4)
 ! 	write(6,*) 'densidade dos conglomerado=',hip(i,1,4),hip(i,2,4)
 ! 	end do




 ! 	write(6,*) '========================'
 ! 	do i=1, ic2(1)
 ! 	write(6,*) 'densidade dos congl-emb1=',hip(i,1,6),hip(i,2,6)
 ! 	end do

 ! 	write(6,*) '========================'
 ! 	do i=1, ic2(2)
 ! 	write(6,*) 'densidade dos congl-emb2=',hip(i,1,7),hip(i,2,7)
 ! 	end do

!!!!!!!! ABRINDO O SEGUNDO CONJUNTO DE DADOS
 ! 	open(2,file='dados_sint_c1.txt')

 ! 	read(2,15) cab    ! cabeçalho
 ! !	write(6,15) cab
 ! 	read(2,15) cab    ! linha em branco abaixo do cabeçalho
 ! !	write(6,15) cab
		
	
 !  do i=1,ntc
 ! 	read(2,*) branco,a1,a2,a3,a4,a5,a6
	
 !  end do
 ! 	close(2)


 !  do i=1,ic1(1)
 !   do j=1,4

 !  lito1(i,j)=hip(i,j+1,1)
 ! end do
 ! end do

 !  lito2(1,1)=a3
 !  lito2(1,2)=a4
 !  lito2(1,3)=a5
 !  lito2(1,4)=a6

 !  do i=1,4
 !  write(6,*) 'primeiro dado a ser classificado=', lito2(1,i)
 !  end do


 !  call maha(lito1,ic1(1),lito2,1,4,dist)
 !  write(6,*) '========================'

 !  write(6,*) 'dist=',dist
 CALL cpu_time(final)
 custocomputacional=final-inicial
 PRINT*, 'Custo Computacional=',custocomputacional, 'segundos'
 PRINT*,' ************ FIM *************'
 
 CONTAINS

!  SUBROUTINE maha(g11,np1,g22,np2,ndim,dist)      
	
!  	subrotina que calcula a distância de mahalanobis entre
!  	dois agrupamentos de elementos com dimensão ndim 	


! 	implicit real*8(a-h,o-z)

!  	real*8,intent(in)::g1(np1,ndim),g2(np2,ndim)
!  	real*8,intent(out)::dist
!  	integer,intent(in)::np1,np2,ndim 


!       real*8 g1(np1,ndim),g2(np2,ndim),g1T(ndim,np1),g2T(ndim,np2),&
!      	cov1(ndim,ndim),cov2(ndim,ndim),covag(ndim,ndim),soma(ndim),&
!         xm1(ndim),xm2(ndim),g22(np2,ndim),&
!        md(ndim,1),mdT(1,ndim),alfa(1,ndim),d2(1,1),g11(np1,ndim)


! 	g1=g11
! 	g2=g22

!  	grupo 1	

! 	do j=1,ndim
! 	soma(j)=0d0
! 	do i=1,np1
! 	soma(j)=soma(j)+g1(i,j)
! 	end do
! 	end do

! 	do i=1,ndim
! 	xm1(i)=soma(i)/dfloat(np1)
! 	end do	

!  	grupo 2	

! 	do j=1,ndim
! 	soma(j)=0d0
! 	do i=1,np2
! 	soma(j)=soma(j)+g2(i,j)
! 	end do
! 	end do


! 	do i=1,ndim
! 	xm2(i)=soma(i)/dfloat(np2)
! 	end do	

!  	vetor das diferenças - será escrito sobre a matrizes g1 e g2

! 	do j=1,ndim
! 	do i=1,np1
! 	g1(i,j)=g1(i,j)-xm1(j)
! 	end do
! 	end do

! 	do j=1,ndim
! 	do i=1,np2
! 	g2(i,j)=g2(i,j)-xm2(j)
! 	end do
! 	end do	

!      --------GRUPO 1 ---------------------
!  	criando a matriz transposta g1T
!  	-------------- -------------------
! 	do i=1,np1    !107 ! número de equações 
! 	do j=1,ndim   !2
! 	g1T(j,i)=g1(i,j)
! 	end do
! 	end do
!  ----------------------------------------------------
!  	 - multiplicação de matrizes
!  	   multiplicação de g1T por g1 

! 	do k=1,ndim
! 	do j=1,ndim
! 	cov1(j,k)=0.d0
! 	do i=1,np1	
! 	cov1(j,k)=cov1(j,k)+g1T(j,i)*g1(i,k)
! 	end do
! 	end do
! 	end do

! 	do i=1,ndim
! 	do j=1,ndim
! 	cov1(i,j)=cov1(i,j)/dfloat(np1)
! 	end do
! 	end do

!  	write(6,*) '======covariância 1 ======'
!  	write(6,*) cov1(1,1),cov1(1,2)
!  	write(6,*) cov1(2,1),cov1(2,2)

!      --------GRUPO 2 ---------------------
!  	criando a matriz transposta g2T

! 	do i=1,np2
! 	do j=1,ndim
! 	g2T(j,i)=g2(i,j)
! 	end do
! 	end do

!  ---------------------------------------------------
!  	 - multiplicação de matrizes
!  	   multiplicação de g2T por g2 

! 	do k=1,ndim
! 	do j=1,ndim
! 	cov2(j,k)=0.d0
! 	do i=1,np2
! 	cov2(j,k)=cov2(j,k)+g2T(j,i)*g2(i,k)
! 	end do
! 	end do
! 	end do

! 	do i=1,ndim
! 	do j=1,ndim
! 	cov2(i,j)=cov2(i,j)/dfloat(np2)
! 	end do
! 	end do

!  	write(6,*) '======covariância 2 ======'
!  	write(6,*) cov2(1,1),cov2(1,2)
!  	write(6,*) cov2(2,1),cov2(2,2)


!  	-------- covariância agrupada------

! 	do i=1,ndim
! 	do j=1,ndim
! 	covag(i,j)=dfloat(np1)*cov1(i,j)/(dfloat(np1+np2))+ &
!                   dfloat(np2)*cov2(i,j)/(dfloat(np1+np2))
! 	end do
! 	end do

!  	write(6,*) '======covariância agrupada ======'
!  	write(6,*) covag(1,1),covag(1,2)
!  	write(6,*) covag(2,1),covag(2,2)	

!  	inversao da matriz covag - usando subrotina

! 	call INVERT(covag,ndim)

!  	write(6,*) '====== inv covariância agrupada ======'
!  	write(6,*) covag(1,1),covag(1,2)
!  	write(6,*) covag(2,1),covag(2,2)

!  	diferenicas médias

! 	do i=1,ndim
! 	md(i,1)=xm1(i)-xm2(i)
! 	end do

!  	write(6,*) '====== diferencias medias ======'
!  	write(6,*) md(1,1)
!  	write(6,*) md(2,1)

!  	criando a matriz transposta mdT
!  	---------------------------
! 	do i=1,ndim
! 	do j=1,1
! 	mdT(j,i)=md(i,j)
! 	end do
! 	end do

!  ----------------------------------------------------
!  	multiplicação de mdT por cov^-1 
!  	 - multiplicação de matrizes

! 	do k=1,ndim	
! 	do j=1,1	
! 	alfa(j,k)=0.d0
! 	do i=1,ndim
! 	alfa(j,k)=alfa(j,k)+mdT(j,i)*covag(i,k)
! 	end do
! 	end do
! 	end do

!  ----------------------------------------------------
!  	multiplicação de alfa por md 
!  	 - multiplicação de matrizes

! 	do k=1,1
! 	do j=1,1
! 	d2(j,k)=0.d0
! 	do i=1,ndim  !2	!
! 	d2(j,k)=d2(j,k)+alfa(j,i)*md(i,k)
! 	end do
! 	end do
! 	end do

! 	dist=dsqrt(d2(1,1))

!       return
!       end
!  END SUBROUTINE maha
!  ! cccccccccccccccccccccccccc


!   SUBROUTINE INVERT(A,i)      
!       real*8 A(i,i),B(i)
!       integer i,im,j,k,l
!       IM=I-1
!       DO 5 K=1,I
!       DO 2 J=1,IM
!     2 B(J)=A(1,J+1)/A(1,1)
!       B(I)=1.d0/A(1,1)
!       DO 4 L=1,IM
!       DO 3 J=1,IM
!     3 A(L,J)=A(L+1,J+1)-A(L+1,1)*B(J)
!     4 A(L,I)=-A(L+1,1)*B(I)
!       DO 5 J=1,I
!     5 A(I,J)=B(J)
 
!       RETURN
!       END
!   END SUBROUTINE INVERT

END PROGRAM preclassificador