#pragma once

struct SourceLocation
{
	SourceLocation(int line, int col = 0)
		: line(line), col(col) {}

	int line;
	int col;
};


extern SourceLocation CurLoc;
extern SourceLocation LexLoc;
