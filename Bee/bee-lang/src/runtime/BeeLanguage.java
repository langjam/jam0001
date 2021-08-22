package runtime;

import ast.comment.CommentsUnit;
import lexer.BeeLexer;
import parser.BeeParser;
import runtime.exceptions.BeeErrorException;
import semantic.BeeResolver;
import token.SourceLocation;
import token.Token;
import utils.FileReader;

import java.util.*;

public class BeeLanguage {

    private final String mainFile;
    private final String sourceDirectory;
    private final Set<String> sourceFiles;

    private int availableCoresNumber;

    private int argc;
    private List<String> argv;

    private boolean showNotes;
    private boolean showAlerts;

    private static final int BEE_LANG_VERSION = 1;

    public BeeLanguage(String mainFile, String sourceDirectory) {
        this.mainFile = mainFile;
        this.sourceDirectory = sourceDirectory;
        this.sourceFiles = new HashSet<>();
        this.sourceFiles.add(mainFile);
        this.availableCoresNumber = Runtime.getRuntime().availableProcessors();
    }

    public String getSourceDirectory() {
        return sourceDirectory;
    }

    public void execute() {
        FileReader fileReader = new FileReader(mainFile);
        BeeLexer beeLexer = new BeeLexer(this, fileReader);

        BeeParser beeParser = new BeeParser(this, beeLexer);
        CommentsUnit commentsUnit = beeParser.parseCommentsUnit();

        BeeInterpreter beeInterpreter = new BeeInterpreter(this);

        BeeResolver beeResolver = new BeeResolver(this, beeInterpreter);
        beeResolver.resolve(commentsUnit);

        beeInterpreter.interpret(commentsUnit);
    }

    public void setArgc(int argc) {
        this.argc = argc;
    }

    public int getArgc() {
        return argc;
    }

    public void setArgv(List<String> argv) {
        this.argv = argv;
    }

    public List<String> getArgv() {
        return argv;
    }

    public void addNewFile(String file) {
        sourceFiles.add(file);
    }

    public boolean isFileParsed(String file) {
        return sourceFiles.contains(file);
    }

    public void setShowNotes(boolean showNotes) {
        this.showNotes = showNotes;
    }

    public boolean shouldShowNotes() {
        return showNotes;
    }

    public void setShowAlerts(boolean show) {
        this.showAlerts = show;
    }

    public boolean shouldShowAlerts() {
        return showAlerts;
    }

    public void setAvailableCoresNumber(int availableCoresNumber) {
        this.availableCoresNumber = availableCoresNumber;
    }

    public int getAvailableCoresNumber() {
        return availableCoresNumber;
    }

    public int getBeeLangVersion() {
        return BEE_LANG_VERSION;
    }

    public BeeErrorException createBeeError(String message, Token token) {
        return new BeeErrorException(message, token.getSourceLocation());
    }

    public BeeErrorException createBeeError(String message, SourceLocation location) {
        return new BeeErrorException(message, location);
    }
}
