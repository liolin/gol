#include <iostream>
#include <vector>
#include <chrono>
#include <thread>

using namespace std;

// a game of live implemenation

void printOut(vector<vector<int> > field)
{
	cout << "\033[2J";
	for(int x = 0; x < field.size(); x++)
	{
		for(int y = 0; y < field[x].size(); y++)
		{
			cout << "\033[" << y+1 << ";" << x+1 << "H" << field[x][y];
		}
	}
	cout << '\n';
}


/*
  what x need:
  - a data structure cell (dead, alivce)
  - a field (vector<vector<cell, cell > >
  - the rules
*/

int main()
{
	// init field
	const int DEAD = 0;
	const int ALIVE = 1;
	const int SIZE = 3;
	vector<vector<int> > field(SIZE, vector<int>(SIZE));
	vector<vector<int> > newField(SIZE, vector<int>(SIZE));

	/*
	  D A D
	  D A D
	  D A D
	 */
	field[0][0] = DEAD;
	field[0][1] = DEAD;
	field[0][2] = DEAD;
	field[1][0] = ALIVE;
	field[1][1] = ALIVE;
	field[1][2] = ALIVE;
	field[2][0] = DEAD;
	field[2][1] = DEAD;
	field[2][2] = DEAD;

	while(true)
	{
		printOut(field);
	
		// start game of live
		for(int x = 0; x < SIZE; x++)
		{
			for(int y = 0; y < SIZE; y++)
			{
				int nOfN = 0;
				int cellState = field[x][y];
				
				// first row
				if(y - 1 >= 0)
				{
					if(x + 1 < 3)
					{
						nOfN += field[x+1][y-1];
					}
				
					if(x-1 >= 0)
					{
						nOfN += field[x-1][y-1];
					}

					nOfN += field[x][y-1];
				}
			
				// second row
				if(x + 1 < 3)
				{
					nOfN += field[x+1][y];
				}
			
				if(x-1 >= 0)
				{
					nOfN += field[x-1][y];
				}
			
				// third row
				if(y + 1 >= 0 && y + 1 < 3)
				{
					if(x + 1 < 3)
					{
						nOfN += field[x+1][y+1];
					}
				
					if(x-1 >= 0)
					{
						nOfN += field[x-1][y+1];
					}

					nOfN += field[x][y+1];
				}				

				if(cellState == DEAD)
				{
					if(nOfN == 3)
					{
						newField[x][y] = ALIVE;
					}
				}
				else
				{
					if(nOfN < 2 || nOfN > 3)
					{
						newField[x][y] = DEAD;
					}
					else
					{
						newField[x][y] = ALIVE;
					}
				}
			}
		}
		// sleep for 1 second
		std::this_thread::sleep_for(std::chrono::milliseconds(500));
		printOut(newField);
		field = newField;
	}
	
	return 0;
}