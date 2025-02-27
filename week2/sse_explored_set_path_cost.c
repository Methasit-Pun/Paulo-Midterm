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
	Heap-allocating state-space exploration agent
*/
struct state_space_node
{
	struct state s;

	//path cost
	int cost;

	struct state_space_node *after_U;
	struct state_space_node *after_D;
	struct state_space_node *after_L;
	struct state_space_node *after_R;
	struct state_space_node *after_C;

	struct state_space_node *parent;
};

struct state_space_visited_node
{
	struct state s;
	struct state_space_visited_node *next;
};

//returns TRUE if state not found in queue
int not_visited(struct state s,struct state_space_visited_node *visited_queue)
{
	while(visited_queue != NULL)
	{
		int grass_match = 1;
		int i, j;
		for(i=0;i<4;i++)
		{
			for(j=0;j<4;j++)
			{
				if(s.grass[s.x][s.y] != visited_queue->s.grass[visited_queue->s.x][visited_queue->s.y])
					grass_match = 0;
			}
		}

		if(s.x != visited_queue->s.x)
		{
			visited_queue = visited_queue->next;
			continue;
		}
		if(s.y != visited_queue->s.y)
		{
			visited_queue = visited_queue->next;
			continue;
		}
		if(!grass_match)
		{
			visited_queue = visited_queue->next;
			continue;
		}
		return 0;
	}
	return 1;
}

/*
	Now must include visited queue, checked before expansion
*/
struct state_space_node *expand_tree(struct state_space_node *node, int depth, struct state_space_visited_node *visited_queue)
{
	if(depth == 0)
		return node;

	if(node == NULL)
		return NULL;

	//expand valid state-space children for current node
	if(node->s.y < 3)
	{
		//but only if not visited yet
		if(not_visited(transition(node->s,U),visited_queue))
		{
			node->after_U = (struct state_space_node *)malloc(sizeof(struct state_space_node));
			node->after_U->s = transition(node->s,U);
			node->after_U->after_U = node->after_U->after_D = node->after_U->after_L = node->after_U->after_R = node->after_U->after_C = NULL;
			node->after_U->parent = node;

			node->after_U->cost = node->cost+2;

			//add it to visited queue before recursion
			struct state_space_visited_node *new_visited = (struct state_space_visited_node *)malloc(sizeof(struct state_space_visited_node));
			new_visited->s = transition(node->s,U);
			new_visited->next = visited_queue;

			node->after_U = expand_tree(node->after_U, depth-1,visited_queue);
		}
	}
	if(node->s.y > 0)
	{
		//but only if not visited yet
		if(not_visited(transition(node->s,U),visited_queue))
		{
			node->after_D = (struct state_space_node *)malloc(sizeof(struct state_space_node));
			node->after_D->s = transition(node->s,D);
			node->after_D->after_U = node->after_D->after_D = node->after_D->after_L = node->after_D->after_R = node->after_D->after_C = NULL;
			node->after_D->parent = node;

			node->after_D->cost = node->cost+2;

			//add it to visited queue before recursion
			struct state_space_visited_node *new_visited = (struct state_space_visited_node *)malloc(sizeof(struct state_space_visited_node));
			new_visited->s = transition(node->s,D);
			new_visited->next = visited_queue;

			node->after_D = expand_tree(node->after_D, depth-1,visited_queue);
		}
	}
	if(node->s.x < 3)
	{
		//but only if not visited yet
		if(not_visited(transition(node->s,U),visited_queue))
		{
			node->after_R = (struct state_space_node *)malloc(sizeof(struct state_space_node));
			node->after_R->s = transition(node->s,R);
			node->after_R->after_U = node->after_R->after_D = node->after_R->after_L = node->after_R->after_R = node->after_R->after_C = NULL;
			node->after_R->parent = node;

			node->after_R->cost = node->cost+2;

			//add it to visited queue before recursion
			struct state_space_visited_node *new_visited = (struct state_space_visited_node *)malloc(sizeof(struct state_space_visited_node));
			new_visited->s = transition(node->s,R);
			new_visited->next = visited_queue;

			node->after_R = expand_tree(node->after_R, depth-1,visited_queue);
		}
	}
	if(node->s.x > 0)
	{
		//but only if not visited yet
		if(not_visited(transition(node->s,U),visited_queue))
		{
			node->after_L = (struct state_space_node *)malloc(sizeof(struct state_space_node));
			node->after_L->s = transition(node->s,L);
			node->after_L->after_U = node->after_L->after_D = node->after_L->after_L = node->after_L->after_R = node->after_L->after_C = NULL;
			node->after_L->parent = node;

			node->after_L->cost = node->cost+2;

			//add it to visited queue before recursion
			struct state_space_visited_node *new_visited = (struct state_space_visited_node *)malloc(sizeof(struct state_space_visited_node));
			new_visited->s = transition(node->s,L);
			new_visited->next = visited_queue;

			node->after_L = expand_tree(node->after_L, depth-1,visited_queue);
		}
	}
	if(node->s.grass[node->s.x][node->s.y] == 1)
	{
		//but only if not visited yet
		if(not_visited(transition(node->s,U),visited_queue))
		{
			node->after_C = (struct state_space_node *)malloc(sizeof(struct state_space_node));
			node->after_C->s = transition(node->s,C);
			node->after_C->after_U = node->after_C->after_D = node->after_C->after_L = node->after_C->after_R = node->after_C->after_C = NULL;
			node->after_C->parent = node;

			node->after_C->cost = node->cost+1;

			//add it to visited queue before recursion
			struct state_space_visited_node *new_visited = (struct state_space_visited_node *)malloc(sizeof(struct state_space_visited_node));
			new_visited->s = transition(node->s,C);
			new_visited->next = visited_queue;

			node->after_C = expand_tree(node->after_C, depth-1,visited_queue);
		}
	}
	return node;
}

struct state_space_node *find_highest_score(struct state_space_node *node, struct state_space_node *best)
{
	if(node == NULL)
		return best;

	struct state_space_node *test = node;
	if((utility(test->s) - 0.2 * test->cost) > (utility(best->s) - 0.2 * best->cost))
		best = test;

	test = find_highest_score(node->after_U, best);
	if((utility(test->s) - 0.2 * test->cost) > (utility(best->s) - 0.2 * best->cost))
		best = test;

	test = find_highest_score(node->after_D, best);
	if((utility(test->s) - 0.2 * test->cost) > (utility(best->s) - 0.2 * best->cost))
		best = test;

	test = find_highest_score(node->after_L, best);
	if((utility(test->s) - 0.2 * test->cost) > (utility(best->s) - 0.2 * best->cost))
		best = test;

	test = find_highest_score(node->after_R, best);
	if((utility(test->s) - 0.2 * test->cost) > (utility(best->s) - 0.2 * best->cost))
		best = test;

	test = find_highest_score(node->after_C, best);
	if((utility(test->s) - 0.2 * test->cost) > (utility(best->s) - 0.2 * best->cost))
		best = test;

	return best;
}

enum action find_action_to_target(struct state_space_node *root, struct state_space_node *target)
{
	if(target->parent == NULL) //if target is root
	{
		//return random action
		return rand()%5;
	}
	while(target->parent != root)
	{
		target = target->parent;
	}

	//Find which action
	if(root->after_U == target)
		return U;
	if(root->after_D == target)
		return D;
	if(root->after_L == target)
		return L;
	if(root->after_R == target)
		return R;
	if(root->after_C == target)
		return C;
}

void clean_tree(struct state_space_node *node)
{
	if(node == NULL)
		return;
	clean_tree(node->after_U);
	clean_tree(node->after_D);
	clean_tree(node->after_L);
	clean_tree(node->after_R);
	clean_tree(node->after_C);
	free(node);
	return;
}

enum action agent(struct state s, int depth)
{
	//Create state-space evolution tree
	struct state_space_node *root = (struct state_space_node *)malloc(sizeof(struct state_space_node));
	root->s = s;
	root->after_U = root->after_D = root->after_L = root->after_R = root->after_C = NULL;
	root->parent = NULL;

	root->cost = 0;

	root = expand_tree(root, depth, NULL); 

	//Find highest scoring node in tree
	struct state_space_node *target = find_highest_score(root, root);

	//Find action that leads towards that node
	enum action todo = find_action_to_target(root, target);

	//Clean tree, return action
	clean_tree(root);

	return todo;
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