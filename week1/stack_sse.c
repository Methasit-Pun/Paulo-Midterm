#include<stdio.h>
#include<stdlib.h>
#include<time.h>

struct state
{
	int grass[4][4];
	//agent position
	int x, y;
};

void print_state(struct state s)
{
	int i, j;

	for(j=3;j>=0;j--)
	{
		for(i=0;i<4;i++)
		{
			printf("%d ",s.grass[i][j]);
		}
		printf("\n");
	}

}

enum action{U,D,L,R,C};
char actions[] = {'U','D','L','R','C'};

struct state init_environment(struct state s)
{
	s.x = s.y = 0;

	int i, j;

	for(i=0;i<4;i++)
	{
		for(j=0;j<4;j++)
		{
			s.grass[i][j] = rand()%2;
		}
	}

	return s;
}

struct state transition(struct state s, enum action a)
{
	switch(a)
	{
	case U:
		if(s.y < 3)
			s.y++;
		break;
	case D:
		if(s.y > 0)
			s.y--;
		break;
	case R:
		if(s.x < 3)
			s.x++;
		break;
	case L:
		if(s.x > 0)
			s.x--;
		break;
	case C:
		s.grass[s.x][s.y] = 0;
		break;
	}
	return s;
}

int utility(struct state s)
{
	int score = 16;
	int i, j;

	for(i=0;i<4;i++)
	{
		for(j=0;j<4;j++)
		{
			if(s.grass[i][j] == 1)
				score--;
		}
	}
	return score;
}



/*
	Stack-allocating state-space exploration agent
*/
int recursive_state_exploration(struct state s, int depth)
{
	int score = utility(s);
	
	if(s.y < 3)
	{
		struct state s_U = transition(s,U);
		if(utility(s_U) > score)
		{
			score = utility(s_U);
		}
		if(depth > 1)
		{
			int recursive_score = recursive_state_exploration(s_U,depth-1);
			if(recursive_score > score)
				score = recursive_score;
		}
	}
	if(s.y > 0)
	{
		struct state s_D = transition(s,D);
		if(utility(s_D) > score)
		{
			score = utility(s_D);
		}
		if(depth > 1)
		{
			int recursive_score = recursive_state_exploration(s_D,depth-1);
			if(recursive_score > score)
				score = recursive_score;
		}
	}
	if(s.x > 0)
	{
		struct state s_L = transition(s,L);
		if(utility(s_L) > score)
		{
			score = utility(s_L);
		}
		if(depth > 1)
		{
			int recursive_score = recursive_state_exploration(s_L,depth-1);
			if(recursive_score > score)
				score = recursive_score;
		}
	}
	if(s.x < 3)
	{
		struct state s_R = transition(s,R);
		if(utility(s_R) > score)
		{
			score = utility(s_R);
		}
		if(depth > 1)
		{
			int recursive_score = recursive_state_exploration(s_R,depth-1);
			if(recursive_score > score)
				score = recursive_score;
		}
	}
	if(s.grass[s.x][s.y] == 1)
	{
		struct state s_C = transition(s,C);
		if(utility(s_C) > score)
		{
			score = utility(s_C);
		}
		if(depth > 1)
		{
			int recursive_score = recursive_state_exploration(s_C,depth-1);
			if(recursive_score > score)
				score = recursive_score;
		}
	}
	return score;
}
enum action agent(struct state s, int depth)
{
	int score = utility(s);
	enum action best = U;
	
	if(s.y < 3)
	{
		struct state s_U = transition(s,U);
		int recursive_score = recursive_state_exploration(s_U,depth-1);
		if(recursive_score > score)
		{
			score = utility(s_U);
			best = U;
		}
	}
	if(s.y > 0)
	{
		struct state s_D = transition(s,D);
		int recursive_score = recursive_state_exploration(s_D,depth-1);
		if(recursive_score > score)
		{
			score = utility(s_D);
			best = D;
		}
	}
	if(s.x > 0)
	{
		struct state s_L = transition(s,L);
		int recursive_score = recursive_state_exploration(s_L,depth-1);
		if(recursive_score > score)
		{
			score = utility(s_L);
			best = L;
		}
	}
	if(s.x < 3)
	{
		struct state s_R = transition(s,R);
		int recursive_score = recursive_state_exploration(s_R,depth-1);
		if(recursive_score > score)
		{
			score = utility(s_R);
			best = R;
		}
	}
	if(s.grass[s.x][s.y] == 1)
	{
		struct state s_C = transition(s,C);
		int recursive_score = recursive_state_exploration(s_C,depth-1);
		if(recursive_score > score)
		{
			score = utility(s_C);
			best = C;
		}
	}
	return best;

}



int main(int argc, char *argv[])
{
	struct state s;

	int depth;

	if(argc != 2)
		return 1;

	depth = atoi(argv[1]);

	srand(time(NULL));

	s = init_environment(s);

	print_state(s);
	printf("Initial score is %d\n",utility(s));

	//let's run 10 iterations
	int i;
	for(i=0 ; i<10; i++)
	{
		enum action a = agent(s, depth);
		printf("At %d,%d, action %c\n",s.x,s.y,actions[a]);
		s = transition(s, a);
	}

	printf("Final score is %d\n",utility(s));

	return 0;
}