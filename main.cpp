#include <iostream>
#include <vector>
#include <chrono>
#include <thread>
#include <string>

using namespace std;

// a game of live implemenation

/**
 * @param vector<vector<int> > field
 *  The field which should be printed
 * @return
 *  void
 */
void printOut(vector<vector<int> > field)
{
	const string NORMAL_SETTINGS = "\033[0m";
	
	// clear
	cout << "\033[2J";
	for(int x = 0; x < field.size(); x++)
	{
		for(int y = 0; y < field[x].size(); y++)
		{
			// write on position x / y
			if(field[x][y] == 1)
			{
				cout << "\033[" << y+1 << ";" << x+1 << "H" << 0;
			}
		}
	}
	cout << NORMAL_SETTINGS;
	cout << '\n';
}

int main()
{
	const int DEAD = 0;
	const int ALIVE = 1;
	int SIZE = 0;

	cin >> SIZE;
	vector<vector<int> > field(SIZE, vector<int>(SIZE));
	vector<vector<int> > newField(SIZE, vector<int>(SIZE));
	const int BOX_SIZE = (SIZE > 3) ? SIZE : 3;


	
	
	// read field configuration from stdin
	for(int x = 0; x < SIZE; x++)
	{
		for(int y = 0; y < SIZE; y++)
		{
			cin >> field[x][y];
		}
	}
	
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
					if(x + 1 < BOX_SIZE)
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
				if(x + 1 < BOX_SIZE)
				{
					nOfN += field[x+1][y];
				}
			
				if(x-1 >= 0)
				{
					nOfN += field[x-1][y];
				}
			
				// third row
				if((y + 1) < BOX_SIZE)
				{
					if(x + 1 < BOX_SIZE)
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
					else
					{
						newField[x][y] = DEAD;
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
		this_thread::sleep_for(chrono::milliseconds(500));
		printOut(newField);
		field = newField;
	}
	
	return 0;
}
