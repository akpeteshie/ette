/*** includes ***/
#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <stdarg.h>
#include <unistd.h>
#include <termios.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <ctype.h>
#include <stdio.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>

/*** defines ***/
#define KILO_VERSION "0.0.1"
#define KILO_TAB_STOP 8
#define KILO_QUIT_TIMES 3

#define CTRL_KEY(k) ((k) & 0x1f)

enum editorKey {
	BACKSPACE = 127,
	ARROW_LEFT = 1000,
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN
};

enum editorHighlight {
	HL_NORMAL = 0,
	HL_COMMENT,
	HL_MLCOMMENT,
	HL_KEYWORD1,
	HL_KEYWORD2,
	HL_STRING,
	HL_NUMBER,
	HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

/*** data ***/

// stores syntax data for specific filetypes
struct editorSyntax {
	char *filetype;
	char **filematch;
	char **keywords;
	char *singleline_comment_start;
	char *multiline_comment_start;
	char *multiline_comment_end;
	int flags;
};

// creating a datatype that represents a row in the editor
typedef struct erow {
	// stores row index at the time of insertion
	int idx;
	// storing the size of the row contente
	int size;
	// storing the size of the actual character to be displayd
	int rsize;
	// storing the characters of the row
	char *chars;
	// storing the actual characters to be displayed
	char *render;
	// stores highlighting of the row
	unsigned char *hl;
	// stores whether row is in a multiline comment
	int hl_open_comment;
} erow;

// creating a global editor datatype
struct editorConfig {
	// storing the cursor position
	int cx, cy;
	// storing the render cursor position
	int rx;
	// storing the row currently scrolled to
	int rowoff;
	// storing the column currently scrolled to
	int coloff;
	// storing the number of rows and cols on the screen
	int screenrows;
	int screencols;
	// storing the number of rows written
	int numrows;
	// storing the information in the rows
	erow *row;
	// storing flag to check whether file has been modified
	int dirty;
	// storing the filename
	char *filename;
	// storing messages to user
	char statusmsg[80];
	time_t statusmsg_time;
	// stores file syntax highlighting info
	struct editorSyntax *syntax;
	// storing the terminal configurations
	struct termios orig_termios;
};

// creating variable to manipulate terminal
struct editorConfig E;

/*** filetypes ***/
char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {
	"switch", "if", "while", "for", "break", "continue", "return", "else", "struct", "union", "typedef", "static", "enum", "class", "case", "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", "void|", NULL
};

struct editorSyntax HLDB[] = {
	{
		"c",
		C_HL_extensions,
		C_HL_keywords,
		"//", "/*", "*/",
		HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
	},
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))


/*** prototypes ***/
void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));

/*** terminal ***/
// functon to process errors
void die(const char *s) {
	// escape command that clears the screen
	write(STDOUT_FILENO, "\x1b[2J", 4);
	//escape command that resets the cursor
	write(STDOUT_FILENO, "\x1b[H", 3);

	perror(s);
	exit(1);
}

// function to turn on echo
void disableRawMode() {
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) die("tcsetattr");
}

// function to enable raw mode
void enableRawMode() {
	// getting the attributes and storing them in orig_termios
	if(tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
	atexit(disableRawMode);

	struct termios raw = E.orig_termios;

	//turning off relevant flags in the terminal 
	raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
	raw.c_oflag &= ~(OPOST);
	raw.c_cflag |= (CS8);
	raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
	raw.c_cc[VMIN] = 0;
	raw.c_cc[VTIME] = 1;

	// setting the attribute back without flags
	if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}


// function to read key input
int editorReadKey() {
	int nread;
	char c;

	// reading the first inputted char
	while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
		if (nread == -1 && errno != EAGAIN) die("read");
	}

	// considering case when inputted char is an escape char
	if (c == '\x1b') {
		char seq[3];

		// reading the command
		if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
		if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

		if (seq[0] == '[') {
			if (seq[1] >= '0' && seq[1] <= '9') {
				if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
				if (seq[2] == '~') {
					switch (seq[1]){
						case '1': return HOME_KEY;
						case '3': return DEL_KEY;
						case '4': return END_KEY;	  
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;	  
					}
				}
			}
			else {
				switch (seq[1]) {
					case 'A': return ARROW_UP;
					case 'B': return ARROW_DOWN;
					case 'C': return ARROW_RIGHT;
					case 'D': return ARROW_LEFT;
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;	  
				}
			}
		} else if (seq[0] == 'O') {
			switch (seq[1]) {
				case 'H': return HOME_KEY;
				case 'F': return END_KEY;	  
			}
		}
		return '\x1b';
	} else {
		return c;
	}
}

// function to 
int getCursorPosition(int *rows, int *cols) {
	char buf[31];
	unsigned int i = 0;

	if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

	while (i < sizeof(buf) - 1) {
		if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
		if (buf[i] == 'R') break;
		i++;
	}
	buf[i] = '\0';

	if (buf[0] != '\x1b' || buf[1] != '[') return -1;
	if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

	return 0;
}

//function to get window size
int getWindowSize(int *rows, int *cols) {
	struct winsize ws;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
		if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
		return getCursorPosition(rows, cols);
		return -1;
	} else {
		*cols = ws.ws_col;
		*rows = ws.ws_row;
		return 0;
	}
}

/*** syntax highlighting ***/

// function that identifies separator characters
int is_separator(int c){
        return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

// function that assigns highlight value to all row chars
void editorUpdateSyntax(erow *row) {
	// makes a length copy of row and populates each char index with color
	row->hl = realloc(row->hl, row->rsize);
	memset(row->hl, HL_NORMAL, row->rsize);

	if (E.syntax == NULL) return;

	char **keywords = E.syntax->keywords;

	char *scs = E.syntax->singleline_comment_start;
	char *mcs = E.syntax->multiline_comment_start;
	char *mce = E.syntax->multiline_comment_end;

	int scs_len = scs ? strlen(scs) : 0;
	int mcs_len = mcs ? strlen(mcs) : 0;
	int mce_len = mce ? strlen(mce) : 0;

        // tracks whether previous char was a separator
        int prev_sep = 1;

	// tracks whether chars are in a string or a multiline comment
	int in_string = 0;
	int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

	// iterates through chars in row and assigns color indexes
	int i = 0;
        while (i < row->size) {
                char c = row->render[i];
                // stores prev char highlighting
                unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

		// processes single line comments
		if (scs_len && !in_string && !in_comment) {
			if (!strncmp(&row->render[i], scs, scs_len)) {
				memset(&row->hl[i], HL_COMMENT, row->rsize - i);
				break;
			}
		}

		// processes multiline comments
		if (mcs_len && mce_len && !in_string) {
			if (in_comment) {
				row->hl[i] = HL_MLCOMMENT;
				if (!strncmp(&row->render[i], mce, mce_len)) {
					memset(&row->hl[i], HL_MLCOMMENT, mce_len);
					i += mce_len;
					in_comment = 0;
					prev_sep = 1;
					continue;
				} else {
					i++;
					continue;
				}
			} else if (!strncmp(&row->render[i], mcs, mcs_len)) {
				memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
				i += mcs_len;
				in_comment = 1;
				continue;
			}
		}

		// assigns highlight values to strings
		if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
			if (in_string) {
				row->hl[i] = HL_STRING;
				if (c == '\\' && i + 1 < row->rsize) {
					row->hl[i + 1] = HL_STRING;
					i += 2;
					continue;
				}
				if (c == in_string) in_string = 0;
				i++;
				prev_sep = 1;
				continue;
			} else {
				if (c == '"' || c == '\'') {
					in_string = c;
					row->hl[i] = HL_STRING;
					i++;
					continue;
				}
			}
		}

		// assigns highlight values to numbers preceded by separator
		if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
			if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) || (c == '.' && prev_hl == HL_NUMBER)) {
				row->hl[i] = HL_NUMBER;
				i++;
				prev_sep = 0;
				continue;
			}
		}

		// highlights keywords
		if (prev_sep) {
			int j;
			for (j = 0; keywords[j]; j++) {
				int klen = strlen(keywords[j]);
				int kw2 = keywords[j][klen - 1] == '|';
				if (kw2) klen--;

				if (!strncmp(&row->render[i], keywords[j], klen) && is_separator(row->render[i + klen])) {
					memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
					i += klen;
					break;
				}
			}
			if (keywords[j] != NULL) {
				prev_sep = 0;
				continue;
			}
		}

                prev_sep = is_separator(c);
                i++;
	}
	// recursively calls itself when highlighing is changed
	int changed = (row->hl_open_comment != in_comment);
	row->hl_open_comment = in_comment;
	if (changed && row->idx + 1 < E.numrows) editorUpdateSyntax(&E.row[row->idx + 1]);
}

// function that maps hl values to color codes
int editorSyntaxToColor(int hl) {
	switch (hl) {
		case HL_COMMENT:
		case HL_MLCOMMENT: return 36;
		case HL_KEYWORD1: return 33;
		case HL_KEYWORD2: return 32;
		case HL_STRING: return 35;
		case HL_NUMBER: return 31;
		case HL_MATCH: return 34;
		default: return 37;
	}
}

// function that matches highlighting to filematch field
void editorSelectSyntaxHighlight() {
	E.syntax = NULL;
	if (E.filename == NULL) return;

	// stores file extension
	char *ext = strrchr(E.filename, '.');

	// loops through syntax highlight database for filetype
	for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
		struct editorSyntax *s = &HLDB[j];
		unsigned int i = 0;
		while (s->filematch[i]) {
			int is_ext = (s->filematch[i][0] == '.');
			// assigns syntax from filematch when filetype matches
			if ((is_ext && ext && !strcmp(ext, s->filematch[i])) || (!is_ext && strstr(E.filename, s->filematch[i]))) {
				E.syntax = s;

				int filerow;
				for (filerow = 0; filerow < E.numrows; filerow++) {
					editorUpdateSyntax(&E.row[filerow]);
				}

				return;
			}
			i++;
		}
	}
}

/*** row operations ***/

// function that converts chars index to render index
int editorRowCxToRx(erow *row, int cx) {
	int rx = 0;
	int j;

	for (j = 0; j < cx; j++) {
		// setting rx to reflect the tab
		if (row->chars[j] == '\t') {
			rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
			rx++;
		}	
	}

	return rx;
}

// function that converts render index to chars index
int editorRowRxToCx(erow *row, int rx) {
	int cur_rx = 0;
	int cx;

	for (cx = 0; cx < row->size; cx++) {
		// setting cur_rx to reflect the tab
		if (row->chars[cx] == '\t') {
			cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
			cur_rx++;
		}	
		if (cur_rx > rx) return cx;
	}

	return cur_rx;
}

// function that populates the render string
void editorUpdateRow(erow *row) {
	int tabs = 0;
	int j;
	
	// counting the tabs in the row
	for (j = 0; j < row->size; j++) if (row->chars[j] == '\t') tabs++;

	free(row->render);
	row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);

	// copying the characters from chars into render with tabs as spaces
	int idx = 0;
	for (j = 0; j < row->size; j++) {
		if (row->chars[j] == '\t') {
			row->render[idx++] = ' ';
			while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
		} else {
			row->render[idx++] = row->chars[j];
		}
	}
	row->render[idx] = '\0';
	row->rsize = idx;

	editorUpdateSyntax(row);
}

// function that adds a row with inputted data
void editorInsertRow(int at, char *s, size_t len) {
	if (at < 0 || at > E.numrows) return;

	// moving all rows from 'at' down a row
	E.row = realloc(E.row, sizeof(erow)*(E.numrows + 1));
	memmove(&E.row[at + 1], &E.row[at], sizeof(erow)*(E.numrows - at));
	// updates idx of all rows below at
	for (int j = at + 1; j <= E.numrows; j++) E.row[j].idx++;

	E.row[at].idx = at;

	// moving inputted data into the last row of the editor
	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1);
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';

	E.row[at].rsize = 0;
	E.row[at].render = NULL;
	E.row[at].hl = NULL;
	E.row[at].hl_open_comment = 0;
	editorUpdateRow(&E.row[at]);

	E.numrows++;
	E.dirty++;
}

// function to free memory of a row
void editorFreeRow(erow *row) {
	free(row->render);
	free(row->chars);
	free(row->hl);
}

// function to delete a row
void editorDelRow(int at) {
	if (at < 0 || at >= E.numrows) return;
	editorFreeRow(&E.row[at]);
	memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
	for (int j = at; j < E.numrows - 1; j++) E.row[j].idx--;
	E.numrows--;
	E.dirty++;
}

// function to insert character into erow
void editorRowInsertChar(erow *row, int at, int c){
	// setting the insertion of the character at the end of the line
	if (at < 0 || at > row->size) at = row->size;
	// reallocating space in memory for inserted character
	row->chars = realloc(row->chars, row->size + 2);
	// pushing everything ahead of at forward by a byte
	memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
	// updating the size to reflect the change 
	row->size++;
	// placing the new character in the empty byte
	row->chars[at] = c;
	// updating render with the new content
	editorUpdateRow(row);
	// updating the dirty flag
	E.dirty++;
}

// function to add a string to the end of a row
void editorRowAppendString(erow *row, char *s, size_t len) {
	row->chars = realloc(row->chars, row->size + len + 1);
	memcpy(&row->chars[row->size], s, len);
	row->size += len;
	row->chars[row->size] = '\0';
	editorUpdateRow(row);
	E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
	if (at < 0 || at >= row->size) return;
	// overwriting the data at 'at' with the data one char over
	memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
	row->size--;
	editorUpdateRow(row);
	E.dirty++;
}

/*** editor operations ***/

// function to insert a character at the cursor

void editorInsertChar(int c) {
	// adding new row if cursor at end of filend of file
	if (E.cy == E.numrows) {
		editorInsertRow(E.numrows, "", 0);
	}
	// inserting character at cursor
	editorRowInsertChar(&E.row[E.cy], E.cx, c);
	// updating cursor position
	E.cx++;
}

// function to add a new line at the cursor
void editorInsertNewline() {
	// adds a new line above cursor
	if (E.cx == 0) {
		editorInsertRow(E.cy, "", 0);
	// splits row at cursor between current and next row
	} else {
		erow *row = &E.row[E.cy];
		editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
		row = &E.row[E.cy];
		row->size = E.cx;
		row->chars[row->size] = '\0';
		editorUpdateRow(row);
	}
	E.cy++;
	E.cx = 0;
}

// function to delete character in editor
void editorDelChar() {
	if (E.cy == E.numrows) return;
	if (E.cx == 0 && E.cy == 0) return;

	// deleting the character at the cursor
	erow *row = &E.row[E.cy];
	if (E.cx > 0) {
		editorRowDelChar(row, E.cx - 1);
		E.cx--;
	// deleting the row if the cursor is at the beginning
	} else {
		E.cx = E.row[E.cy - 1].size;
		editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
		editorDelRow(E.cy);
		E.cy--;
	}
}

/*** file i/o ***/

// saving file to disk
char *editorRowsToString(int *buflen) {
	int totlen = 0;
	int j;
	// adding up the lengths of all rows of text including the newline char at the end
	for (j = 0; j < E.numrows; j++) 
		totlen += E.row[j].size + 1;
	*buflen = totlen;


	// copying file into buf using p as an indexer
	char *buf = malloc(totlen);
	char *p = buf;
	for (j = 0; j < E.numrows; j++) {
		memcpy(p, E.row[j].chars, E.row[j].size);
		p += E.row[j].size;
		*p = '\n';
		p++;
	}

	return buf;
}

// function to open and read files from disk
void editorOpen(char *filename) {
	// copying the filename into E.filename
	free(E.filename);
	E.filename = strdup(filename);

	editorSelectSyntaxHighlight();

	// opening the file
	FILE *fp = fopen(filename, "r");
	if (!fp) die("fopen");

	// storing the contents of the file
	char *line = NULL;
	size_t linecap = 0;
	ssize_t linelen;
	// iterating through the file line by line
	while((linelen = getline(&line, &linecap, fp)) != -1) {
		while (linelen > 0 && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r')) linelen--;
		editorInsertRow(E.numrows, line, linelen);
	}
	free(line);
	fclose(fp);
	E.dirty = 0;
}

// function that writes file to disk
void editorSave() {
	// if the file is new save as
	if (E.filename == NULL) {
		E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
		if (E.filename == NULL){
			editorSetStatusMessage("Save aborted");
			return;
		}
		editorSelectSyntaxHighlight();
	}

	// storing the contents of the file
	int len;
	char *buf = editorRowsToString(&len);

	// opening the file for reading and writing
	int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
	if (fd != -1) {
		if (ftruncate(fd, len) != -1) {
			if (write(fd, buf, len) == len) {
				close(fd);
				free(buf);
				E.dirty = 0;
				editorSetStatusMessage("%d bytes written to disk", len);
				return;
				}
			}
			close(fd);
		}
		free(buf);
		editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/

// callback function for incremental searching
void editorFindCallback(char *query, int key) {
	// sets up direction of search
	static int last_match = -1;
	static int direction = 1;

	static int saved_hl_line;
	static char *saved_hl = NULL;

	// restores coloring
	if (saved_hl) {
		memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
		free(saved_hl);
		saved_hl = NULL;
	}

	if (key == '\r' || key == '\x1b') {
		last_match = -1;
		direction = 1;
		return;
	} else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
		direction = 1;
	} else if (key == ARROW_LEFT || key == ARROW_UP) {
		direction = -1;
	} else {
		last_match = -1;
		direction = 1;
	}

	if (last_match == -1) direction = 1;
	// stores current row index being searched from
	int current = last_match;

	// iterates through lines
	int i;
	for (i = 0; i < E.numrows; i++) {
		// wraps document from end to beginning and vice versa
		current += direction;
		if (current == -1) current = E.numrows - 1;
		else if (current == E.numrows) current = 0;

		erow *row = &E.row[current];
		// checks whether query is a substring of current row and returns pointer to matching substring
		char *match = strstr(row->render, query);
		// moves cursor and viewport to queried word.
		if (match) {
			// stores index of last match found
			last_match = current;
			E.cy = current;
			E.cx = editorRowRxToCx(row, match - row->render);
			E.rowoff = E.numrows;

			// saves prior color config
			saved_hl_line = current;
			saved_hl = malloc(row->size);
			memcpy(saved_hl, row->hl, row->size);

			// colors the match
			memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
			break;
		}
	}

}

// function that searches for words
void editorFind() {
	// saving cursor position before search
	int saved_cx = E.cx;
	int saved_cy = E.cy;
	int saved_coloff = E.coloff;
	int saved_rowoff = E.rowoff;

	// queries user for word
	char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", editorFindCallback);

	if (query) {
		free(query);
	} else {
		E.cx = saved_cx;
		E.cy = saved_cy;	
		E.coloff = saved_coloff;
		E.rowoff = saved_rowoff;	
	}
}

/*** append buffer ***/

// append buffer data type with pointer and length
struct abuf {
	char *b;
	int len;
};

// constructor for abuf
#define ABUF_INIT {NULL, 0}

// adding data to a buffer
void abAppend(struct abuf *ab, const char *s, int len) {
	// adding space of size len to the buffer
	char *new = realloc(ab->b, ab->len + len);

	// returning if the buffer failed to be allocated
	if (new == NULL) return;

	// copying s to the added space in buffer
	memcpy(&new[ab->len], s, len);

	// adjusting the buffer to reflect the addition
	ab->b = new;
	ab->len += len;
}

void abFree(struct abuf *ab) {
	free(ab->b);
}

/*** output ***/

// funtion to adjust editor view based on cursor position
void editorScroll() {
	E.rx = 0;
	if (E.cy < E.numrows) {
		E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
	}
	
	// checking whether cursor is above visible window
	if (E.cy < E.rowoff) {
		E.rowoff = E.cy;
	}

	// checking whether cursor is below visible window
	if (E.cy >= E.rowoff + E.screenrows) {
		E.rowoff = E.cy - E.screenrows + 1;
	}
	// checking whether cursor is to the left of visible window
	if (E.rx < E.coloff) {
		E.coloff = E.rx;
	}
	// checking whether cursor is to the right of visible window
	if (E.rx >= E.coloff + E.screencols) {
		E.coloff = E.rx - E.screencols + 1;
	}
}

// function to draw rows in editor
void editorDrawRows(struct abuf *ab) {
	int y;
	for (y = 0; y < E.screenrows; y++) {
		// checking whether y is past the written rows
		int filerow = y + E.rowoff;
		if (filerow >= E.numrows) {
			// printing welcome message if there are no written rows
			if (E.numrows == 0 && y == E.screenrows / 3){
				char welcome[80];
				int welcomelen = snprintf(welcome, sizeof(welcome), "Kilo editor -- version %s", KILO_VERSION);
				if (welcomelen > E.screencols) welcomelen = E.screencols;

				// centering the message
				int padding = (E.screencols - welcomelen) / 2;
				if (padding) {
					abAppend(ab, "~", 1);
					padding--;
				}
				while (padding--) abAppend(ab, " ", 1);
				abAppend(ab, welcome, welcomelen);
			} else {
				// putting a tilde on unwritten rows
				abAppend(ab, "~", 1);
			}
		} else {
			// adding the rows to be shown to the buffer
			int len = E.row[filerow].rsize - E.coloff;
			if (len < 0) len = 0;
			if (len > E.screencols) len = E.screencols;
			// storing row to be rendered and highlighting info
			char *c = &E.row[filerow].render[E.coloff];
			unsigned char *hl = &E.row[filerow].hl[E.coloff];
			int current_color = -1;
			int j;
			// applies coloring
			for (j = 0; j < len; j++) {
				// adds printable character to control characters
				if (iscntrl(c[j])) {
					char sym = (c[j] <= 26) ? '@' + c[j] : '?';
					abAppend(ab, "\x1b[7m", 4);
					abAppend(ab, &sym, 1);
					abAppend(ab, "\x1b[m", 3);
					if (current_color != -1) {
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
						abAppend(ab, buf, clen);
					}
				} else if (hl[j] == HL_NORMAL) {
					if (current_color != -1) {
						abAppend(ab, "\x1b[39m", 5);
						current_color = -1;
					}
					abAppend(ab, &c[j], 1);
				} else {
					int color = editorSyntaxToColor(hl[j]);
					if (color != current_color) {
						current_color = color;
						char buf[16];
						int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
						abAppend(ab, buf, clen);
					}
					abAppend(ab, &c[j], 1);
				}
			}
			abAppend(ab, "\x1b[39m", 5);
		}

		// clearing the line
		abAppend(ab, "\x1b[K", 3);

		abAppend(ab, "\r\n", 2);
	}
}

// function that draws the status bar
void editorDrawStatusBar(struct abuf *ab) {
	// escape sequence to invert colors
	abAppend(ab, "\x1b[7m]", 4);
	// creating arrays to store strings to be displayed in status bar
	char status[80], rstatus[80];
	// displaying file info 
	int len = snprintf(status, sizeof(status), "%.20s - %d lines %s", E.filename ? E.filename : "[No Name]", E.numrows, E.dirty? "(modified)" : "");
	// displaying current line number
	int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d", E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
	if (len > E.screencols) len = E.screencols;
	abAppend(ab, status, len);
	while (len < E.screencols) {
		if (E.screencols - len == rlen) {
		abAppend(ab, rstatus, rlen);
		break;
		} else {
			abAppend(ab, " ", 1);
			len++;
		}
	}
	// escape sequence to reset color formatting
	abAppend(ab, "\x1b[m", 3);

	abAppend(ab,"\r\n", 2);
}

// functin that draws message bar
void editorDrawMessageBar(struct abuf *ab) {
	// clearing message bar
	abAppend(ab, "\x1b[K", 3);
	int msglen = strlen(E.statusmsg);
	// cutting the message if it is longer than the screen
	if (msglen > E.screencols) msglen = E.screencols;
	if (msglen && time(NULL) - E.statusmsg_time < 5) abAppend(ab, E.statusmsg, msglen);
}

// function to refresh screen
void editorRefreshScreen() {
	editorScroll();

	struct abuf ab = ABUF_INIT;

	// escape sequence to hide cursor
	abAppend(&ab, "\x1b[?25l", 6);
	//escape command that resets the cursor
	abAppend(&ab, "\x1b[H", 3);

	editorDrawRows(&ab);
	editorDrawStatusBar(&ab);
	editorDrawMessageBar(&ab);

	// positioning the cursor based on the values of E.cx and E.cy
	char buf[32];
	snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1, (E.rx - E.coloff) + 1);
	abAppend(&ab, buf, strlen(buf));

	abAppend(&ab, "\x1b[H", 3);
	// escape sequence to show cursor
	abAppend(&ab, "\x1b[?25h", 6);

	write(STDOUT_FILENO, ab.b, ab.len);
	abFree(&ab);
}

// function to take string and display as status message
void editorSetStatusMessage(const char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
	va_end(ap);
	E.statusmsg_time = time(NULL);
}

/*** input ***/

// function that prompts the user for action
char *editorPrompt(char *prompt, void (*callback)(char *, int)){
	// stores user input
	size_t bufsize = 128;
	char *buf = malloc(bufsize);

	//indexes the user input
	size_t buflen = 0;
	buf[0] = '\0';

	// processes user action
	while (1) {
		editorSetStatusMessage(prompt, buf);
		editorRefreshScreen();

		int c = editorReadKey();
		// implements backspace
		if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
			if (buflen != 0) buf[--buflen] = '\0';
		// cancels save w/ ESC
		} else if (c == '\x1b') {
			editorSetStatusMessage("");
			if (callback) callback(buf, c);
			free(buf);
			return NULL;
		// returns input w/ ENTER
		} else if (c == '\r') {
			if (buflen != 0) {
				editorSetStatusMessage("");
				if (callback) callback(buf, c);
				return buf;
			}
		// stores the user input if valid
		} else if (!iscntrl(c) && c < 128) {
			// doubles buffersize if limit reached
			if (buflen == bufsize - 1) {
				bufsize *= 2;
				buf = realloc(buf, bufsize);
			}
			// updates buffer
			buf[buflen++] = c;
			buf[buflen] = '\0';
		}

		if (callback) callback(buf, c);
	}
}


// function to move cursor
void editorMoveCursor(int key) {
	// storing the current cursor row
	erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];

	switch (key) {
		case ARROW_LEFT:
			if (E.cx != 0) {
				E.cx--;
			} else if (E.cy > 0) {
				E.cy--;
				E.cx = E.row[E.cy].size;
			}
			break;
		case ARROW_RIGHT:
			if (row && E.cx < row->size) {
				E.cx++;
			} else if (row && E.cx == row->size) {
				E.cy++;
				E.cx = 0;
			}
			break;
		case ARROW_UP:
			if (E.cy != 0) {
				E.cy--;
			}
			break;
		case ARROW_DOWN:
			if (E.cy < E.numrows) {
				E.cy++;
			}
			break;
	}

	// adjusting the cursor limit based on the size of the current cursor row
	row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
	int rowlen = row ? row->size : 0;
	if (E.cx > rowlen) {
		E.cx = rowlen;
	}
}

// function to process different key presses
void editorProcessKeypress() {
	static int quit_times = KILO_QUIT_TIMES;

	int c = editorReadKey();

	switch (c) {
		case '\r':
			editorInsertNewline();
			break;

		// command sequence to quit
		case CTRL_KEY('q'):
			// warning the user of losing unsaved changes
			if (E.dirty && quit_times > 0) {
				editorSetStatusMessage("WARNING!!! File has unsaved changes. Press Ctrl-Q %d more times to quit.", quit_times);
				quit_times--;
				return;
			}
			write(STDOUT_FILENO, "\x1b[2J", 4);
			write(STDOUT_FILENO, "\x1b[H", 3);
			exit(0);
			break;

		case CTRL_KEY('s'):
			editorSave();
			break;

		case HOME_KEY:
			E.cx = 0;
			break;

		case END_KEY:
			if (E.cy < E.numrows) E.cx = E.row[E.cy].size;
			break;

		case CTRL_KEY('f'):
			editorFind();
			break;

		case BACKSPACE:
		case CTRL_KEY('h'):
		case DEL_KEY:
			if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
			editorDelChar();
			break;
		case PAGE_UP:
		case PAGE_DOWN:
			{
				// setting the cursor at the top and bottom of the screen
				if (c == PAGE_UP) {
					E.cy = E.rowoff;
				} else if (c == PAGE_DOWN) {
					E.cy = E.rowoff + E.screenrows - 1;
					if (E.cy > E.numrows) E.cy = E.numrows;
				}

				int times = E.screenrows;
				while (times--) {editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);}
			} 
			break;
		case ARROW_UP:
		case ARROW_DOWN:
		case ARROW_LEFT:
		case ARROW_RIGHT:
			editorMoveCursor(c);
			break;
			
		case CTRL_KEY('l'):
		case '\x1b':
			break;


		default:
			editorInsertChar(c);
			break;
	}

	quit_times = KILO_QUIT_TIMES;
}

/*** init ***/
// initializes E
void initEditor() {
	// establishing editor values
	E.cx = 0;
	E.cy = 0;
	E.rx = 0;
	E.rowoff = 0;
	E.coloff = 0;
	E.numrows = 0;
	E.row = NULL;
	E.dirty = 0;
	E.filename = NULL;
	E.statusmsg[0] = '\0';
	E.statusmsg_time = 0;
	E.syntax = NULL;

	if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
	// decrementing screenrows to save line for status bar
	E.screenrows -= 2;
}

int main(int argc, char *argv[]) {
	// enable raw mode
	enableRawMode();
	// initialize editor properties
	initEditor();
	// reading file input
	if (argc >= 2) {
		editorOpen(argv[1]);
	}

	editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

	// running the editor
	while (1) {
		editorRefreshScreen();
		editorProcessKeypress();
	}
		return 0;
}

 
 
