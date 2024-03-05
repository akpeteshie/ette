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

enum editorkey {
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

/*** data ***/

// creating a datatype that represents a row in the editor
typedef struct erow {
	// storing the size of the row contente
	int size;
	// storing the size of the actual character to be displayd
	int rsize;
	// storing the characters of the row
	char *chars;
	// storing the actual characters to be displayed
	char *render;
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
	// storing the terminal configurations
	struct termios orig_termios;
};

// creating variable to manipulate terminal
struct editorConfig E;


/*** prototypes ***/
void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt);

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
}

// function that adds a row with inputted data
void editorInsertRow(int at, char *s, size_t len) {
	if (at < 0 || at > E.numrows) return;

	// moving all rows from 'at' down a row
	E.row = realloc(E.row, sizeof(erow)*(E.numrows + 1));
	memmove(&E.row[at + 1], &E.row[at], sizeof(erow)*(E.numrows - at));

	// moving inputted data into the last row of the editor
	E.row[at].size = len;
	E.row[at].chars = malloc(len + 1);
	memcpy(E.row[at].chars, s, len);
	E.row[at].chars[len] = '\0';

	E.row[at].rsize = 0;
	E.row[at].render = NULL;
	editorUpdateRow(&E.row[at]);

	E.numrows++;
	E.dirty++;
}

// function to free memory of a row
void editorFreeRow(erow *row) {
	free(row->render);
	free(row->chars);
}

// function to delete a row
void editorDelRow(int at) {
	if (at < 0 || at >= E.numrows) return;
	editorFreeRow(&E.row[at]);
	memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
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
		E.filename = editorPrompt("Save as: %s (ESC to cancel)");
		if (E.filename == NULL){
			editorSetStatusMessage("Save aborted");
			return;
		}
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
			abAppend(ab, &E.row[filerow].render[E.coloff], len);
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
	int rlen = snprintf(rstatus, sizeof(rstatus), "%d/%d", E.cy + 1, E.numrows);
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
char *editorPrompt(char *prompt){
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
			free(buf);
			return NULL;
		// returns input w/ ENTER
		} else if (c == '\r') {
			if (buflen != 0) {
				editorSetStatusMessage("");
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

	editorSetStatusMessage("HELP: Ctrl-S = save | Ctrl-Q = quit");

	// running the editor
	while (1) {
		editorRefreshScreen();
		editorProcessKeypress();
	}
		return 0;
}

 
 
