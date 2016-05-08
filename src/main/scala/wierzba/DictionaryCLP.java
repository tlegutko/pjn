package wierzba;

import com.sun.jna.Library;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DictionaryCLP {

    private final CLibraryCLP libInstance;

    /**
     * Creates java facade for C library CLP.
     * Remember about magic lp1a26.db and lp1b26.db files required in your /usr/local/clp/db directory.
     * And remember to have JNA dependency in your classpath.
     *
     * @param clpLibraryPath path to directory containing libclp_2.6.so
     */
    public DictionaryCLP(String clpLibraryPath) {
        System.setProperty("jna.library.path", clpLibraryPath);
        libInstance = (CLibraryCLP) Native.loadLibrary("clp_2.6", CLibraryCLP.class);
        libInstance.clp_init(1); // utf-8, 0 for ISO-8859-2
    }

    public DictionaryCLP() {
        this("src/main/resources/wierzba");
    }

    private interface CLibraryCLP extends Library {
        void clp_init(int enc);

        void clp_ver(Pointer p);

        int clp_stat(int pos);

        void clp_rec(String inp, Pointer out, IntByReference num);

        int clp_pos(int id);

        void clp_label(int id, Pointer out);

        void clp_bform(int id, Pointer out);

        void clp_forms(int id, Pointer out);

        void clp_formv(int id, Pointer out);

        void clp_vec(int id, String inp, Pointer out, IntByReference num);

        // following are outside of documentation scope

        int clp_pid(int id);

        void clp_plain(String inp, Pointer out);

        void clp_ogonki(String inp, Pointer out);

        void clp_orec(String inp, Pointer out, IntByReference num);
    }

    public enum WordType {
        RZECZOWNIK,
        CZASOWNIK,
        PRZYMIOTNIK,
        LICZEBNIK,
        ZAIMEK,
        PRZYSLOWEK,
        WYKRZYKNIK,
        PRZYIMEK,
        SPOJNIK,
        NIEODMIENNY,
        SKROT;

        public static final WordType values[] = values();

        /** Index starting from 1, as in CLP lib. */
        public static WordType fromInt(int index) {
            return values[index-1];
        }
    }

    public String clp_ver() {
        Pointer ptr = allocateCharArray();
        libInstance.clp_ver(ptr);
        return ptr.getString(0);
    }

    public int clp_stat(int pos) {
        return libInstance.clp_stat(pos);
    }

    public List<Integer> clp_rec(String word) {
        Pointer out = allocateIntArray();
        IntByReference size = new IntByReference();
        libInstance.clp_rec(word, out, size);
        return pointersToListOfInts(out, size);
    }

    public WordType clp_pos(int id) {
        return WordType.fromInt(libInstance.clp_pos(id));
    }

    public List<String> clp_formv(int id) {
        Pointer out = allocateCharArray();
        libInstance.clp_formv(id, out);
        return pointerToListOfStrings(out);
    }

    /**
     * Returns all distinct forms
     */
    public List<String> clp_forms(int id) {
        Pointer out = allocateCharArray();
        libInstance.clp_forms(id, out);
        return pointerToListOfStrings(out);
    }

    public List<String> clp_bform(int id) {
        Pointer out = allocateCharArray();
        libInstance.clp_bform(id, out);
        return pointerToListOfStrings(out);
    }

    public String clp_label(int id) {
        Pointer out = allocateCharArray();
        libInstance.clp_label(id, out);
        return out.getString(0);
    }

    /**
     * Returns positions in clp_formv vector, indexed from 1.
     */
    public List<Integer> clp_vec(int id, String word) {
        Pointer out = allocateIntArray();
        IntByReference size = new IntByReference();
        libInstance.clp_vec(id, word, out, size);
        return pointersToListOfInts(out, size);
    }

    public List<String> clp_ogonki(String word) {
        Pointer out = allocateCharArray();
        libInstance.clp_ogonki(word, out);
        return pointerToListOfStrings(out);
    }

    public String clp_plain(String word) {
        Pointer out = allocateCharArray();
        libInstance.clp_plain(word, out);
        return out.getString(0);
    }

    /**
     * Requires additional lp1c database.
     */
    public int clp_pid(int id) {
        return libInstance.clp_pid(id);
    }

    /**
     * Requires additional lp1d database.
     */
    public List<Integer> clp_orec(String word) {
        Pointer out = allocateIntArray();
        IntByReference size = new IntByReference();
        libInstance.clp_orec(word, out, size);
        return pointersToListOfInts(out, size);
    }

    private Pointer allocateIntArray() {
        return new Memory(20 * Native.getNativeSize(Integer.TYPE));
    }

    private Pointer allocateCharArray() {
        return new Memory(256 * Native.getNativeSize(Character.TYPE));
    }

    private List<String> pointerToListOfStrings(Pointer ptr) {
        return Arrays.asList(ptr.getString(0).split(":"));
    }

    private List<Integer> pointersToListOfInts(Pointer ptr, IntByReference size) {
        int[] ints = ptr.getIntArray(0, size.getValue());
        return new ArrayList<Integer>() {{
            for (int i : ints) add(i);
        }};
    }

    public static void main(String[] args) {
        DictionaryCLP dictionaryCLP = new DictionaryCLP();
        int kluczId = 17240752;
        System.out.println("BASIC CLP TESTING:");
        System.out.println("ver: " + dictionaryCLP.clp_ver());
        System.out.println("all entries num: " + dictionaryCLP.clp_stat(0));
        System.out.println("multiple ids: " + dictionaryCLP.clp_rec("klucz"));
        System.out.println("pos: " + dictionaryCLP.clp_pos(kluczId));
        System.out.println("label: " + dictionaryCLP.clp_label(kluczId));
        System.out.println("basic forms: " + dictionaryCLP.clp_bform(kluczId));
        System.out.println("all distinct forms: " + dictionaryCLP.clp_forms(kluczId));
        System.out.println("all forms: " + dictionaryCLP.clp_formv(kluczId));
        System.out.println("positions in vector" + dictionaryCLP.clp_vec(kluczId, "klucze"));
        System.out.println("plain: " + dictionaryCLP.clp_plain("ęąęówźż"));
        System.out.println("ogonki: " + dictionaryCLP.clp_ogonki("ta"));

        // te dwia ponizej nie maja jakichs dodatkowych baz lp1c i lp1d, na wierzbie ich nie znalazłem (find / 2>/dev/null -name "*lp1*")
//        System.out.println("pid: " + dictionaryCLP.clp_pid(17240752));
//        System.out.println("orec: " + dictionaryCLP.clp_orec("klopot"));
    }

}