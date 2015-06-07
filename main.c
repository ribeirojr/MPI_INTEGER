/*******************************************************
Calculo da Integral Numerica com a Regra do Trapezio
Autor: Moises Ribeiro Jr, 2007

Resumo: Este aplicativo é capaz de fazer uma aproximação
da integral de uma função avaliada de um determinado 
ponto até outro, atraves da Regra do Trapezio. A função,
ponto de inicio do intervalo, final e o intervalo entre
as amostras são informados pelo usuario.
********************************************************/

#include <mpi.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>

#define TAG_OUT 0 //identificador das mensagens de saida
#define TAG_IN 1 //identificador das mensagens de entrada

char separado[50][50]; 

//Essa função é responsável por avaliar a fórmula informada
//pelo usuario para um valor de x que é informado pelo sistema
//e retorna o f(x). (a variavel k é uma variavel temporaria)
float calcular(float x, int k)
{
	while( k > 1)
	{
		//O sistema substitui os X da formula pelo seu valor
		for(int i=0;i<k;i++)
		{
			if(separado[i][0] == 'x')
			{
				char temp[50];
				sprintf(separado[i],"%lf",x);
			}
		}

		//O sistema procura por `^` e eleva o numero anterior ao simbolo
		//pelo número posterior a ele.
		for(int i=0;i<k;i++)
		{
			if(separado[i][0] == '^')
			{
				float a = atof(separado[i-1]);
				float b = atof(separado[i+1]);
				float c = pow(a,b);
				sprintf(separado[i-1],"%lf",c);
				for(int j=i+2;j<k;j++)
				{
					strcpy(separado[j-2],separado[j]);
				}
				k=k-2;
				i=0;
			}
		}

		//O sistema faz as divisões e multiplicações encontradas
		for(int i=0;i<k;i++)
		{
			if(separado[i][0] == '*' || separado[i][0] == '/')
			{
				float a = atof(separado[i-1]);
				float b = atof(separado[i+1]);
				float c;

				if(separado[i][0] == '*')
				c = a*b;
				else
				c = a/b;

				sprintf(separado[i-1],"%lf",c);
				for(int j=i+2;j<k;j++)
				{
					strcpy(separado[j-2],separado[j]);
				}
				k=k-2;
				i=0;
			}
		}

		//O sistema faz as somas e subtrações.
		for(int i=0;i<k;i++)
		{
			if(separado[i][0] == '+' || separado[i][0] == '-')
			{
				float a = atof(separado[i-1]);
				float b = atof(separado[i+1]);
				float c;

				if(separado[i][0] == '+')
				c = a+b;
				else
				c = a-b;

				sprintf(separado[i-1],"%lf",c);
				for(int j=i+2;j<k;j++)
				{
					strcpy(separado[j-2],separado[j]);
				}
				k=k-2;
				i=0;
			}
		}
	}
	float ret = atof(separado[0]);
	return ret;
}


//Essa função é responsável por dividir a formula 
//informada pelo usuario em uma matriz de strings 
//para poder ser avaliada posteriormente e verificar
//a validade da mesma.
int criaFormula(char formula[50])
{
	int i=0,k=0,j=0;
	while(formula[i] != '\0')
	{
		//Se o elemento avaliado for um simbolo, adiciona a matriz e 
		//encerra o elemento.
		if(formula[i] == 'x' || formula[i] == '+' || formula[i] == '-' ||
		formula[i] == '*' || formula[i] == '/' || formula[i] == '^')
		{
			separado[k][0] = formula[i];
			separado[k][1] = '\0';
			k++;
			j=0;

		}

		//Se o elemento for um numero, procura por outros numeros para 
		//completa-lo, até que não haja mais nenhum numero, encerra o 
		//elemento.
		if( formula[i] == '0' || formula[i] == '1' || formula[i] == '2' ||
		formula[i] == '3' || formula[i] == '4' || formula[i] == '5' ||
		formula[i] == '6' || formula[i] == '7' || formula[i] == '8' ||
		formula[i] == '9' || formula[i] == '.' )
		{
			separado[k][j] = formula[i];
			j++;

			if (!(formula[i+1] == '0' || formula[i+1] == '1' || formula[i+1] == '2' ||
			formula[i+1] == '3' || formula[i+1] == '4' || formula[i+1] == '5' ||
			formula[i+1] == '6' || formula[i+1] == '7' || formula[i+1] == '8' ||
			formula[i+1] == '9' || formula[i+1] == '.' ))
			{
				separado[k][j] = '\0';
				k++;
			}
		}
		i++;
	}
	return k;
}


//Função responsável por calcular a area do trapezio, recebendo como parametro
//a formula da função, o inicio e fim do intervalo, e o tamanho das amostras.
double calcula(double inicio, double final, double intervalo, char formula[50])
{
	double total= 0; 
	double i = inicio; 
	double a , b = intervalo , c , area; 
	int size = criaFormula(formula);

	//Para cada intervalo, o sistema calcula o valor do lado A,
	//adiciona ao lado ponto a, avalia o novo ponto para calcular
	//o lado B, faz o calculo da area do trapezio e adiciona as 
	//outras areas já calculada.
	while ( i <= final) 
	{
		a = calcular(i,size);
		criaFormula(formula);
		i = i + intervalo;
		c = calcular(i,size);
		criaFormula(formula);

		area = (b * ( c + a )) / 2;
		total = area + total;
	}
	return total;
}


//Método de execução principal, responsável pela divisão das cargas entre os 
//processos e rotinas para os processos que serão executados.
void main(int argc, char **argv)
{
	int mytid; //Variavel responsavel por armazenar o rank do processo
	int numprocs; //Numero de processos
	double total; //Variavel que armazena o total da soma das areas
	double soma=0; //Variavel que armazena a soma final de todas as areas
	double t0; //Variavel que guarda o tempo de inicio de execucao

	MPI_Status rc; //Variavel responsavel por armazenar o status das mensagens enviadas/recebidas
	MPI_Init(&argc,&argv); //Inicializa o ambiente de programação MPI
	MPI_Comm_rank(MPI_COMM_WORLD,&mytid); //Grava o RANK do processo na variavel mytid
	MPI_Comm_size(MPI_COMM_WORLD,&numprocs); //Grava o numero do processo

	//Verifica se o processo é o pai (rank = 0) ou filho (rank > 0)
	if(mytid == 0)
	{
		//Metodo para o processo pai
		//Recebe os parametros do usuario, envia para os processos,
		//Calcula o seu intervalo.
		double inicio=0, final=0, intervalo; 
		char formula[50];

		fprintf(stdout, "Digite a formula: ");
		fflush(stdout);
		scanf("%s", &formula);

		fprintf(stdout, "Digite o inicio do intervalo: ");
		fflush(stdout);
		scanf("%lf", &inicio);

		fprintf(stdout, "Digite o final do intervalo: ");
		fflush(stdout);
		scanf("%lf", &final);

		fprintf(stdout, "Digite o tamanho das amostras: ");
		fflush(stdout);
		scanf("%lf", &intervalo);

		//Inicia o contador de tempo
		t0 = MPI_Wtime();

		//Faz um broadcast (ou seja, envia para todos os processos) a formula informada pelo usuario
		MPI_Bcast( &formula , 50 , MPI_CHAR , TAG_OUT , MPI_COMM_WORLD );
		//Faz um broadcast (ou seja, envia para todos os processos) o inicio do intervalo
		MPI_Bcast( &inicio , 1 , MPI_DOUBLE , TAG_OUT , MPI_COMM_WORLD );

		//Faz um broadcast (ou seja, envia para todos os processos) o final do intervalo
		MPI_Bcast( &final , 1 , MPI_DOUBLE , TAG_OUT , MPI_COMM_WORLD );
		//Faz um broadcast (ou seja, envia para todos os processos) o tamanho das amostras
		MPI_Bcast( &intervalo , 1 , MPI_DOUBLE , TAG_OUT , MPI_COMM_WORLD );

		//Calcula o tamanho do intervalo informado pelo usuario
		int inter = final - inicio;

		//divide o intervalo pelo numero de processos para definir o intervalo que o processo 
		//um irá calcular, e adiciona o resto para que os outros intervalos precisem só de calcular
		//a divisao do intervalo pelos processos.
		final = inter / numprocs + inter%numprocs;

		//Calcula o seu intervalo
		total = calcula(inicio, final , intervalo , formula);
	} 
	else
	{ 

		//Metodo para os processos Filhos
		double inicio, final, intervalo; 
		char formula[50];

		//Recebe a formula.
		MPI_Bcast( &formula , 50 , MPI_CHAR , TAG_OUT , MPI_COMM_WORLD );
		//Recebe o inicio do intervalo
		MPI_Bcast( &inicio , 1 , MPI_DOUBLE , TAG_OUT , MPI_COMM_WORLD );

		//Recebe o final do intervalo
		MPI_Bcast( &final , 1 , MPI_DOUBLE , TAG_OUT , MPI_COMM_WORLD );
		//Recebe o tamnho das amostras do intervalo
		MPI_Bcast( &intervalo , 1 , MPI_DOUBLE , TAG_OUT , MPI_COMM_WORLD );

		//Calcula o tamanho do intervalo
		int inter = final - inicio;

		//Calcula o inicio do intervalo, se o processo for RANK 1, então o 
		//inicio será igual ao final do processo de RANK 0, e o mesmo calculo
		//para definir o final do intervalo do RANK 0 é feito. Para os outros 
		//RANKS os intervalos são multiplicados pelo numero de RANKs anteriores
		//e adicionado o resto que foi calculado pelo RANK 0.
		int i = mytid;
		if(i==1)
		{
			inicio = (inter / numprocs) + inter%numprocs;
		}
		else
		{
			inicio = (inter / numprocs) * (i) + inter%numprocs;
		}

		//O final do intervalo é sempre o intervalo / numero de processos
		//multiplicado pelo RANK + 1 e adicionado do resto.
		final = (inter / numprocs) * (i+1) + inter%numprocs;

		//Calcula as areas no intervalo definido acima.
		total = calcula( inicio , final , intervalo , formula );
	}

	//Função MPI que envia e armazena os valores dos TOTAIS de cada um 
	//processo. Esse comando é executado até que cada um dos processos
	//tenha retornado seu valor.
	MPI_Reduce(&total, &soma, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

	//Se o RANK do processo for igual a ZERO o sistema 
	//finaliza a execução mostrando os resultados.
	if(mytid == 0)
	{ 
		//Recebe o tempo final da execução
		double t1 = MPI_Wtime();

		//Calcula a diferenca de do tempo inicial com o final
		double time = t1-t0;
		//Mostra o tempo de processamento.
		printf("Tempo de processamento: %.3f\n",time);

		//Mostra a area total calculada
		printf("A area eh: %.3lf \n",soma); 
	}

	MPI_Finalize(); // Comando de finalizacao do ambiente MPI
}