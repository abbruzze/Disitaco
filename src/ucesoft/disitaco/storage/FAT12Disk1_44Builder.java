package ucesoft.disitaco.storage;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Adapted from a ChatGPT prompt result.
 * 
 * @author ChatGPT
 * Created on 16/06/2025 15:54
 */
public class FAT12Disk1_44Builder {
    private static final int SECTOR_SIZE = 512;
    private static final int ROOT_ENTRIES = 224;
    private static final int BYTES_PER_DIR_ENTRY = 32;
    private static final int SECTORS_PER_FAT = 9;
    private static final int RESERVED_SECTORS = 1;
    private static final int NUM_FATS = 2;
    private static final int ROOT_DIR_SECTORS = (ROOT_ENTRIES * BYTES_PER_DIR_ENTRY) / SECTOR_SIZE;
    private static final int CLUSTER_SIZE = SECTOR_SIZE; // 1 sector per cluster
    private static final int _1440 = 1440 * 1024;

    private final File baseDir;
    private byte[][] sectors;
    private int totalSectors;
    private byte[] fat;

    private final List<File> fileList = new ArrayList<>();
    private final Map<File, Integer> fileClusterStart = new HashMap<>();

    public FAT12Disk1_44Builder(File baseDir) {
        this.baseDir = baseDir;
        scanFiles();
        calculateSize();
        build();
    }

    public byte[] getSector(int n) {
        return (n >= 0 && n < sectors.length) ? sectors[n] : new byte[SECTOR_SIZE];
    }

    private void scanFiles() {
        File[] files = baseDir.listFiles();
        if (files != null) {
            var totalSize = 0;
            for (File f : files) {
                if (f.isFile()) {
                    totalSize += (int)f.length();
                    if (totalSize < _1440) fileList.add(f);
                    else break;
                }
            }
        }
    }

    private void calculateSize() {
        int clustersNeeded = 0;
        for (File f : fileList) {
            int clusters = (int) ((f.length() + CLUSTER_SIZE - 1) / CLUSTER_SIZE);
            clustersNeeded += clusters;
        }
        int dataSectors = clustersNeeded;
        totalSectors = RESERVED_SECTORS + (NUM_FATS * SECTORS_PER_FAT) + ROOT_DIR_SECTORS + dataSectors;
        sectors = new byte[totalSectors][SECTOR_SIZE];
        fat = new byte[SECTORS_PER_FAT * SECTOR_SIZE];
    }

    private void build() {
        writeBootSector();
        writeFAT();
        writeRootDirectory();
        writeFileData();
        duplicateFAT();
    }

    private void writeBootSector() {
        byte[] bpb = sectors[0];
        bpb[0] = (byte) 0xEB; bpb[1] = 0x3C; bpb[2] = (byte) 0x90;
        System.arraycopy("MSDOS5.0".getBytes(), 0, bpb, 3, 8);
        bpb[11] = (byte) 0x00; bpb[12] = 0x02; // 512 byte/sector
        bpb[13] = 0x01; // 1 sector/cluster
        bpb[14] = 0x01; // reserved
        bpb[16] = 0x02; // number of FATs
        bpb[17] = (byte) 0xE0; bpb[18] = 0x00; // 224 root entries
        bpb[19] = (byte) (totalSectors & 0xFF); bpb[20] = (byte) ((totalSectors >> 8) & 0xFF);
        bpb[21] = (byte) 0xF0; // media descriptor
        bpb[22] = 0x09; bpb[23] = 0x00; // sectors per FAT
        bpb[24] = 0x12; bpb[25] = 0x00; // sectors per track (optional)
        bpb[26] = 0x02; bpb[27] = 0x00; // heads
        bpb[510] = 0x55;
        bpb[511] = (byte) 0xAA;
    }

    private void writeFAT() {
        // Init media descriptor
        fat[0] = (byte) 0xF0;
        fat[1] = (byte) 0xFF;
        fat[2] = (byte) 0xFF;

        int cluster = 2;
        for (File f : fileList) {
            int clusters = (int) ((f.length() + CLUSTER_SIZE - 1) / CLUSTER_SIZE);
            fileClusterStart.put(f, cluster);
            for (int i = 0; i < clusters; i++) {
                int entry = (i == clusters - 1) ? 0xFFF : cluster + 1;
                writeFAT12Entry(cluster, entry);
                cluster++;
            }
        }

        for (int i = 0; i < SECTORS_PER_FAT; i++) {
            System.arraycopy(fat, i * SECTOR_SIZE, sectors[1 + i], 0, SECTOR_SIZE);
        }
    }

    private void duplicateFAT() {
        for (int i = 0; i < SECTORS_PER_FAT; i++) {
            System.arraycopy(sectors[1 + i], 0, sectors[1 + SECTORS_PER_FAT + i], 0, SECTOR_SIZE);
        }
    }

    private void writeRootDirectory() {
        byte[] root = new byte[ROOT_DIR_SECTORS * SECTOR_SIZE];
        int index = 0;
        for (File f : fileList) {
            byte[] entry = buildDirectoryEntry(f, fileClusterStart.get(f));
            System.arraycopy(entry, 0, root, index * 32, 32);
            index++;
        }

        for (int i = 0; i < ROOT_DIR_SECTORS; i++) {
            System.arraycopy(root, i * SECTOR_SIZE, sectors[1 + NUM_FATS * SECTORS_PER_FAT + i], 0, SECTOR_SIZE);
        }
    }

    private void writeFileData() {
        int dataStart = RESERVED_SECTORS + (NUM_FATS * SECTORS_PER_FAT) + ROOT_DIR_SECTORS;
        for (File f : fileList) {
            int cluster = fileClusterStart.get(f);
            try (InputStream in = new FileInputStream(f)) {
                byte[] buf = new byte[CLUSTER_SIZE];
                int len;
                while ((len = in.read(buf)) != -1) {
                    byte[] sector = sectors[dataStart + (cluster - 2)];
                    System.arraycopy(buf, 0, sector, 0, len);
                    cluster++;
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private byte[] buildDirectoryEntry(File file, int startCluster) {
        byte[] entry = new byte[32];
        String name = file.getName().toUpperCase();
        String base = "        ", ext = "   ";
        int dot = name.lastIndexOf('.');
        if (dot > 0) {
            base = String.format("%-8s", name.substring(0, dot)).substring(0, 8);
            ext = String.format("%-3s", name.substring(dot + 1)).substring(0, 3);
        } else {
            base = String.format("%-8s", name).substring(0, 8);
        }

        System.arraycopy(base.getBytes(), 0, entry, 0, 8);
        System.arraycopy(ext.getBytes(), 0, entry, 8, 3);
        entry[11] = 0x20; // archive
        long size = file.length();
        entry[26] = (byte) (startCluster & 0xFF);
        entry[27] = (byte) ((startCluster >> 8) & 0xFF);
        entry[28] = (byte) (size & 0xFF);
        entry[29] = (byte) ((size >> 8) & 0xFF);
        entry[30] = (byte) ((size >> 16) & 0xFF);
        entry[31] = (byte) ((size >> 24) & 0xFF);
        return entry;
    }

    private void writeFAT12Entry(int cluster, int value) {
        int offset = (cluster * 3) / 2;
        if ((cluster & 1) == 0) {
            fat[offset] = (byte) (value & 0xFF);
            fat[offset + 1] = (byte) ((fat[offset + 1] & 0xF0) | ((value >> 8) & 0x0F));
        } else {
            fat[offset] = (byte) ((fat[offset] & 0x0F) | ((value << 4) & 0xF0));
            fat[offset + 1] = (byte) ((value >> 4) & 0xFF);
        }
    }
}

