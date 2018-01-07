PROGRAM preclassificador
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !Criação da subrotina do cálculo de distância por Mahalanobis                !
  !Orientador: Cosme Ferreira da Ponte Neto                                    !
  !Aluno: Victor Ribeiro Carreira                                              !
  !Este programa visa criar agrupamento de dados de litologias de poços        !
  !Cálculo de distâncias em um                                                 !
  !Subrotina Mahalanobis                                                       !
  !Para usar compilação com flags utilize:                                     !
  !gfortran -fbounds-check -fbacktrace -Wall -Wextra -pedantic                 !
  !"pasta/subpasta/nomedopragrama.f95" -o nomedoexecutável                     !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!



                  !++++++++++++++++Tabela de Variáveis+++++++++++++++!
                  !i - contador dimensinal dos vetores               !
                  !ij - contador diimensional do nt e ntc            !
                  !nt - dimensão dos dados de treinamento            !
                  !ntc - dimensão dos dados de classificação         !
                  !a - armazena codigo, prof, dens, gama, rho e vel  !
                  !cl(i) - vetor de codigo                           !
                  !prof(i) - vetor de profundidade                   !
                  !tr(nt,4) - matriz de densidade                    !
                  !tr(i,2) - matriz de raio-gama                     !
                  !tr(i,3) - matriz de resistividade                 !
                  !tr(i,4) - matriz de velocidade                    !
                  !lito1(:,:) - matriz de litologias                 !
                  !lito2(:,:) - matriz do padrão sino                !
                  !hip(nt,5,9) - hipermatriz com todos os dados      !
                  !ndim - número de dimensões analisadas do modelo   !
                  !       ou número de propriedades físicas.         !
                  !--------------------------------------------------!


 IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!! DECLARAÇÃO DAS VARIÁVEIS GLOBAIS !!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

  INTEGER(KIND=DP):: i, j, ij, nt, ntc, nlito, ndim, k, it, erro
  REAL(KIND=DP):: a1, a2, a3, a4, a5, a6, dist, dist_min
  REAL(KIND=SP):: inicial, final, custocomputacional

  INTEGER(KIND=DP), ALLOCATABLE, DIMENSION(:):: ic1, ic2, kmin, contador
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: prof, cl , distC
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:)::tr, lito1, lito2, dadosC
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:,:)::hip

  CHARACTER(LEN=80):: rocha

    TYPE lixo
     CHARACTER(LEN= 15) :: PALAVRA(7)
    END TYPE lixo

  TYPE(lixo):: cabecalho, branco


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 PRINT*,'*****************************************************************************'
 PRINT*,'************************************INÍCIO***********************************'
 PRINT*,'*****************************************************************************'
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!!!!!!!!!!!!!!!!CRIANDO OS ARQUIVOS E OS FORMATOS  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 CALL cpu_time(inicial)



 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 !!!!!!!!!!!!!!!!!!!!!!!!!!ARMAZENANDO AS VARIÁVEIS DE ENTRADA !!!!!!!!!!!!!!!!!!
 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 CALL entrada


   ! Preenchendo a hipermatriz
  DO i=1,nt
    DO j=1,5                         !Bloco reservado para os 5 padrões caixa
     IF(cl(i) == j)THEN
      ic1(j)=ic1(j)+1
      hip(ic1(j),1,j)=prof(i)
      hip(ic1(j),2,j)=tr(i,1)
      hip(ic1(j),3,j)=tr(i,2)
      hip(ic1(j),4,j)=tr(i,3)
      hip(ic1(j),5,j)=tr(i,4)
     END IF
    END DO
      nlito=450                      !Bloco reservado para o padrão sino
    DO j=1,4
      nlito=nlito+1
      IF(cl(i) == nlito)THEN
       ic2(j)=ic2(j)+1
       hip(ic2(j),1,5+j)=prof(i)
       hip(ic2(j),2,5+j)=tr(i,1)
       hip(ic2(j),3,5+j)=tr(i,2)
       hip(ic2(j),4,5+j)=tr(i,3)
       hip(ic2(j),5,5+j)=tr(i,4)
      END IF
    END DO

  END DO

    ALLOCATE(lito1(ic1(1),4),lito2(1,4),distC(SIZE(hip,3) ), contador(ntc) )

    lito1=0d0
    lito2=0d0
    erro=0d0
  CALL estatistica

  !Criando um arquivo de saída
  OPEN(2,file='SemelhançaC2.txt')
  !19 FORMAT(A4, 4X, A4, 4X, A5, 4X, A5)
  20 FORMAT(F8.1,4x,F15.5,4x,F8.2,4x,I4)

DO it=1,ntc

! ---- propriedades fisicas do arquivo a ser classificado:
    lito2(1,1)=dadosC(it,1)
    lito2(1,2)=dadosC(it,2)
    lito2(1,3)=dadosC(it,3)
    lito2(1,4)=dadosC(it,4)

 ! Laço de cada litotipo (automação das distancias via subroutine maha):
  DO k=1, SIZE(hip,3) ! = SIZE(hip(1,1,:) )
    DO i=1,ic1(1)
     DO j=1,4
      lito1(i,j)=hip(i,j+1,k)
     END DO
    END DO

    !DO i=1,4
    !  WRITE(6,*) 'primeiro dado a ser classificado=', lito2(1,i)
    !END DO

    CALL maha(lito1,ic1(1),lito2,1,4,dist)
 !   WRITE(6,*) '======================='

    distC(k) = dist
  !  PRINT*, 'dist_maha =',distC(k),k

  ENDDO ! laço over k (every lithotype)

   ! localizando a menor distancia e o respectivo litotipo:
   kmin(it) = MINLOC(distC,1) !Retorna o menor valor de distC
   dist_min = MINVAL(distC,1)



  !Calculando os erros de classificação
   IF (dadosC(it,6) /= kmin(it)) THEN
     erro=erro+1
   END IF




!print*,'it=', it, 'indice=',kmin(it), 'distancia=',dist
!WRITE(2,19) 'Prof, Maha, Poco, Class'
WRITE(2,20) dadosC(it,5), dist, dadosC(it,6), kmin(it)  ! Escreve o arquivo de saída semelhança

END DO


! ---- propriedades fisicas do arquivo a ser classificado:
    lito2(1,1)=a3
    lito2(1,2)=a4
    lito2(1,3)=a5
    lito2(1,4)=a6

 ! Laço de cada litotipo (automação das distancias via subroutine maha):
  DO k=1, SIZE(hip,3) ! = SIZE(hip(1,1,:) )
    DO i=1,ic1(1)
     DO j=1,4
      lito1(i,j)=hip(i,j+1,k)
     END DO
    END DO


    CALL maha(lito1,ic1(1),lito2,1,4,dist)
    !WRITE(6,*) '======================='

    distC(k) = dist
    !PRINT*, 'dist_maha =',distC(k),k

  ENDDO ! laço over k (every lithotype)

   ! localizando a menor distancia e o respectivo litotipo:
   kmin = MINLOC(distC,1)!Retorna o menor valor de distC
   dist_min = MINVAL(distC,1)

  !WRITE(6,*) '========================'
  PRINT*, 'Menor distância de mahalanobis encontrada->',dist_min!,kmin
  PRINT*,'Erro->',erro


   WRITE(6,*) '======================================================'
   CALL cpu_time(final)
   custocomputacional=final-inicial
   PRINT*, 'Custo Computacional=',custocomputacional, 'segundos'

 PRINT*,'*****************************************************************************'
 PRINT*,'********************************* FIM ***************************************'
 PRINT*,'*****************************************************************************'

 CONTAINS

!----------------------------------------------------------------------------------------
  SUBROUTINE maha(g11,np1,g22,np2,ndim,dist)

!  	subrotina que calcula a distância de mahalanobis entre
!  	dois agrupamentos de elementos com dimensão ndim

   IMPLICIT NONE
    INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
    INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

    INTEGER(KIND=DP), INTENT(IN):: np1
    INTEGER(KIND=SP), INTENT(IN):: np2, ndim
    REAL(KIND=DP),INTENT(OUT):: dist

    INTEGER(KIND=SP):: i,j,k
    REAL(KIND=DP),ALLOCATABLE,DIMENSION(:)::soma, xm1, xm2
    REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:):: g1, g2, g1T, g2T, cov1, cov2, &
    covag, g11, g22, md, mdT, alfa, d2

    ALLOCATE(soma(ndim),xm1(ndim),xm2(ndim))

    ALLOCATE(g1(np1,ndim),g2(np2,ndim),g1T(ndim,np1),g2T(ndim,np2),&
    cov1(ndim,ndim),cov2(ndim,ndim),covag(ndim,ndim),md(ndim,1),&
    mdT(1,ndim),alfa(1,ndim),d2(1,1))

    g1=g11
    g2=g22

!  	grupo 1

  DO j=1,ndim
    soma(j)=0d0
    DO i=1,np1
      soma(j)=soma(j)+g1(i,j)
    END DO
  END DO

  DO i=1,ndim
    xm1(i)=soma(i)/dfloat(np1)
  END DO

!  	grupo 2

  DO j=1,ndim
    soma(j)=0d0
    DO i=1,np2
      soma(j)=soma(j)+g2(i,j)
    END DO
  END DO

  DO i=1,ndim
    xm2(i)=soma(i)/dfloat(np2)
  END DO

!  	vetor das diferenças - será escrito sobre a matrizes g1 e g2


  DO j=1,ndim
    DO i=1,np1
      g1(i,j)=g1(i,j)-xm1(j)
    END DO
  END DO

  DO  j=1,ndim
    DO i=1,np2
      g2(i,j)=g2(i,j)-xm2(j)
    END DO
  END DO

!      --------GRUPO 1 ---------------------
!  	criando a matriz transposta g1T
!  	-------------- -------------------
  DO i=1,np1    !107 ! número de equações
    DO j=1,ndim   !2
      g1T(j,i)=g1(i,j)
    END DO
  END DO
!  ----------------------------------------------------
!  	 - multiplicação de matrizes
!  	   multiplicação de g1T por g1

  DO k=1,ndim
    DO j=1,ndim
      cov1(j,k)=0.d0
      DO i=1,np1
        cov1(j,k)=cov1(j,k)+g1T(j,i)*g1(i,k)
      END DO
    END DO
  END DO

  DO i=1,ndim
    DO j=1,ndim
      cov1(i,j)=cov1(i,j)/dfloat(np1)
    END DO
  END DO

!  	write(6,*) '======covariância 1 ======'
!  	write(6,*) cov1(1,1),cov1(1,2)
!  	write(6,*) cov1(2,1),cov1(2,2)

!      --------GRUPO 2 ---------------------
!  	criando a matriz transposta g2T

  DO i=1,np2
    DO j=1,ndim
      g2T(j,i)=g2(i,j)
    END DO
  END DO

!  ---------------------------------------------------
!  	 - multiplicação de matrizes
!  	   multiplicação de g2T por g2

   DO k=1,ndim
     DO j=1,ndim
       cov2(j,k)=0.d0
       DO i=1,np2
         cov2(j,k)=cov2(j,k)+g2T(j,i)*g2(i,k)
       END DO
     END DO
   END DO

   DO  i=1,ndim
     DO j=1,ndim
       cov2(i,j)=cov2(i,j)/dfloat(np2)
     END DO
   END DO

   ! WRITE(6,*) '======covariância 2 ======'
   ! WRITE(6,*) cov2(1,1),cov2(1,2)
   ! WRITE(6,*) cov2(2,1),cov2(2,2)


!  	-------- covariância agrupada------

   DO i=1,ndim
     DO j=1,ndim
       covag(i,j)=dfloat(np1)*cov1(i,j)/(dfloat(np1+np2))+ &
       dfloat(np2)*cov2(i,j)/(dfloat(np1+np2))
     END DO
   END DO

    !WRITE(6,*) '======covariância agrupada ======'
    !WRITE(6,*) covag(1,1),covag(1,2)
    !WRITE(6,*) covag(2,1),covag(2,2)

!  	inversao da matriz covag - usando subrotina

   CALL INVERT(covag,ndim)

    !WRITE(6,*) '====== inv covariância agrupada ======'
    !WRITE(6,*) covag(1,1),covag(1,2)
    !WRITE(6,*) covag(2,1),covag(2,2)

!  	diferenicas médias

   DO i=1,ndim
     md(i,1)=xm1(i)-xm2(i)
   END DO

!  	write(6,*) '====== diferencias medias ======'
!  	write(6,*) md(1,1)
!  	write(6,*) md(2,1)

!  	criando a matriz transposta mdT
!  	---------------------------

  DO i=1,ndim
    DO j=1,1
      mdT(j,i)=md(i,j)
    END DO
  END DO

!  ----------------------------------------------------
!  	multiplicação de mdT por cov^-1
!  	 - multiplicação de matrizes


  DO k=1,ndim
    DO j=1,1
      alfa(j,k)=0.d0
      DO i=1,ndim
        alfa(j,k)=alfa(j,k)+mdT(j,i)*covag(i,k)
      END DO
    END DO
  END DO

!  ----------------------------------------------------
!  	multiplicação de alfa por md
!  	 - multiplicação de matrizes

  DO k=1,1
    DO j=1,1
      d2(j,k)=0.d0
      DO i=1,ndim  !2	!
        d2(j,k)=d2(j,k)+alfa(j,i)*md(i,k)
      END DO
    END DO
  END DO

  dist=dsqrt(d2(1,1))


END SUBROUTINE maha


!--------------------------------------------------------------------------


   SUBROUTINE INVERT(A,i)
      integer i,im,j,k,l
      real*8 A(i,i),B(i)

       IM=I-1

       DO 5 K=1,I
         DO 2 J=1,IM
           2 B(J)=A(1,J+1)/A(1,1)
           B(I)=1.d0/A(1,1)
           DO 4 L=1,IM
             DO 3 J=1,IM
               3 A(L,J)=A(L+1,J+1)-A(L+1,1)*B(J)
               4 A(L,I)=-A(L+1,1)*B(I)
               DO 5 J=1,I
                 5 A(I,J)=B(J)

   END SUBROUTINE INVERT


!-------------------------------------------------------------------------

   SUBROUTINE entrada




  OPEN(1,file='dados_sint_T1.txt') ! entrada do programa
   ! Leitura do arquivo de treinamento

   READ(1,15) cabecalho    ! leitura do cabeçalho
   !WRITE(6,15) cabecalho
   READ(1,15) branco    ! linha em branco abaixo do cabeçalho
   !WRITE(6,15) branco

    ij=1
     DO WHILE (.TRUE.)
     READ(1,*,end=6) rocha, a1, a2, a3, a4, a5, a6
     ij=ij+1
     END DO
     6 CONTINUE
  CLOSE(1)

   nt=ij-1
  WRITE(6,*) "n de dados de treinamento->",nt

 ! !!!!!!!!!!!!!
 ! !        Leitura do arquivo de dados a serem classificados


 OPEN(2,file='dados_sint_c2.txt')
  READ(2,15) cabecalho    ! cabeçalho
  !WRITE(6,15) cabecalho
  READ(2,15) branco    ! linha em branco abaixo do cabeçalho
  !WRITE(6,15) branco

   ij=1
   DO WHILE (.TRUE.)
    READ(2,*,END=7) rocha,a1,a2,a3,a4,a5,a6
    ij=ij+1
   END DO
   7 CONTINUE
 CLOSE(2)

   ntc=ij-1
   WRITE(6,*) "n de dados a serem classificados->",ntc


   ALLOCATE(tr(nt,4),cl(nt),prof(nt),hip(nt,5,9),ic1(5),ic2(4),kmin(ntc),&
   dadosC(ntc,6))


   hip=0d0
   ic1=0
   ic2=0
   ndim=4

   dadosC=0d0

OPEN(2,file='dados_sint_c2.txt')
READ(2,15) cabecalho    ! cabeçalho
!WRITE(6,15) cabecalho
READ(2,15) branco    ! linha em branco abaixo do cabeçalho
!WRITE(6,15) branco

DO i=1,ntc
 READ(2,*) rocha,a1,a2,a3,a4,a5,a6
  dadosC(i,1)=a3
  dadosC(i,2)=a4
  dadosC(i,3)=a5
  dadosC(i,4)=a6
  dadosC(i,5)=a2
  dadosC(i,6)=a1
END DO


  OPEN(1,file='dados_sint_T1.txt')

    READ(1,15) cabecalho    ! cabeçalho
    !WRITE(6,15) cabecalho
    READ(1,15) branco    ! linha em branco abaixo do cabeçalho
    !WRITE(6,15) branco

    DO i=1,nt
     READ(1,*) rocha,cl(i),prof(i),tr(i,1),tr(i,2),tr(i,3),tr(i,4)
    END DO
  CLOSE(1)

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

   END SUBROUTINE entrada
!-----------------------------------------------------------------------------------------

   SUBROUTINE estatistica

    !WRITE(6,*) '========================'

    PRINT*, 'ic1=',size(ic1,1) !Tamanho das demais litologias
    PRINT*, 'ic2=',size(ic2,1) !Tamanho da gradação do padrão sino
    PRINT*,'Dimensão da hipermatriz=',SIZE(hip(:,1,1)),SIZE(hip(1,:,1)),SIZE(hip(1,1,:))


    !WRITE(6,*) '========================'

    WRITE(6,*) 'n de folhelhos=',ic1(1)
    WRITE(6,*) 'n de dolomitas=',ic1(2)
    WRITE(6,*) 'n de congl-emb1=',ic2(1)

  !  WRITE(6,*) '========================'

  !   DO i=1,ic1(1)
  !    WRITE(6,*) 'densidade dos folhelhos=',hip(i,1,1),hip(i,2,1)
  !   END DO

  !   WRITE(6,*) '========================'

  !   DO i=1, ic1(2)
  !    WRITE(6,*) 'densidade das dolomitas=',hip(i,1,2),hip(i,2,2)
  !   END DO

  !   WRITE(6,*) '========================'

  !   DO i=1,ic1(3)
  !    WRITE(6,*) 'densidade dos diabasio=',hip(i,1,3),hip(i,2,3)
  !  END DO

  !  WRITE(6,*) '========================'

  !  DO i=1,ic1(4)
  !   WRITE(6,*) 'densidade dos conglomerado=',hip(i,1,4),hip(i,2,4)
  !  END DO

  !  WRITE(6,*) '========================'

  !   DO i=1, ic2(1)
  !    WRITE(6,*) 'densidade dos congl-emb1=',hip(i,1,6),hip(i,2,6)
  !   END DO

  !  WRITE(6,*) '========================'

  !   DO i=1, ic2(2)
  !    WRITE(6,*) 'densidade dos congl-emb2=',hip(i,1,7),hip(i,2,7)
  !   END DO

  !  WRITE(6,*) '========================'

   END SUBROUTINE estatistica

!-------------------------------------------------------------------------------

   SUBROUTINE euclideana()
     IMPLICIT NONE


   END SUBROUTINE euclideana

END PROGRAM preclassificador
