package ucesoft.disitaco.storage;

import java.io.*;
import java.time.Instant;
import java.time.ZoneId;
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
    
    public FAT12Disk1_44Builder(File baseDir) {
        this.baseDir = baseDir;
        var files = scanFiles();
        calculateSize(files);
        build(files);
    }

    public byte[] getSector(int n) {
        return (n >= 0 && n < sectors.length) ? sectors[n] : new byte[SECTOR_SIZE];
    }

    public void setSector(int n,byte[] sec) {
        if (n >= 0 && n < sectors.length) {
            sectors[n] = sec;
        }
    }

    private List<File> scanFiles() {
        List<File> fileList = new ArrayList<>();
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
        return fileList;
    }

    private void calculateSize(List<File> fileList) {
//        int clustersNeeded = 0;
//        for (File f : fileList) {
//            int clusters = (int) ((f.length() + CLUSTER_SIZE - 1) / CLUSTER_SIZE);
//            clustersNeeded += clusters;
//        }
        int dataSectors = (1440 * 1024 + CLUSTER_SIZE - 1) / CLUSTER_SIZE;
        totalSectors = RESERVED_SECTORS + (NUM_FATS * SECTORS_PER_FAT) + ROOT_DIR_SECTORS + dataSectors;
        sectors = new byte[totalSectors][SECTOR_SIZE];
        fat = new byte[SECTORS_PER_FAT * SECTOR_SIZE];
    }

    private void build(List<File> fileList) {
        writeBootSector();
        var fileClusterStart = writeFAT(fileList);
        writeRootDirectory(fileList,fileClusterStart);
        writeFileData(fileList,fileClusterStart);
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

    private Map<File, Integer> writeFAT(List<File> fileList) {
        // Init media descriptor
        fat[0] = (byte) 0xF0;
        fat[1] = (byte) 0xFF;
        fat[2] = (byte) 0xFF;

        Map<File, Integer> fileClusterStart = new HashMap<>();
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
        
        return fileClusterStart;
    }

    private void duplicateFAT() {
        for (int i = 0; i < SECTORS_PER_FAT; i++) {
            System.arraycopy(sectors[1 + i], 0, sectors[1 + SECTORS_PER_FAT + i], 0, SECTOR_SIZE);
        }
    }

    private void writeRootDirectory(List<File> fileList,Map<File, Integer> fileClusterStart) {
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

    private void writeFileData(List<File> fileList,Map<File, Integer> fileClusterStart) {
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

        var data = Instant.ofEpochMilli(file.lastModified()).atZone(ZoneId.systemDefault()).toLocalDateTime();
        int dosTime = (data.getHour() << 11) | (data.getMinute() << 5) | (data.getSecond() / 2);
        int dosDate = ((data.getYear() - 1980) << 9) | (data.getMonthValue() << 5) | data.getDayOfMonth();

        System.arraycopy(base.getBytes(), 0, entry, 0, 8);
        System.arraycopy(ext.getBytes(), 0, entry, 8, 3);
        entry[11] = 0x20; // archive
        entry[14] = (byte) (dosTime & 0xFF);
        entry[15] = (byte) ((dosTime >> 8) & 0xFF);
        entry[16] = (byte) (dosDate & 0xFF);
        entry[17] = (byte) ((dosDate >> 8) & 0xFF);
        entry[18] = entry[16];
        entry[19] = entry[17];
        entry[22] = entry[14];
        entry[23] = entry[15];
        entry[24] = entry[16];
        entry[25] = entry[17];
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

    private int readFAT12Entry(int cluster) {
        int fatOffset = (cluster * 3) / 2;
        int fatStartSector = 1; // FAT1 starts at sector 1 (LBA)
        int sectorIndex = fatStartSector + (fatOffset / 512);
        int offsetInSector = fatOffset % 512;

        byte[] sector = getSector(sectorIndex);
        byte b1 = sector[offsetInSector];
        byte b2 = getSector(sectorIndex + ((offsetInSector + 1) / 512))[(offsetInSector + 1) % 512];

        int value;
        if ((cluster & 1) == 0) {
            value = ((b1 & 0xFF) | ((b2 & 0x0F) << 8));
        } else {
            value = (((b1 & 0xF0) >> 4) | ((b2 & 0xFF) << 4));
        }

        return value & 0xFFF;
    }

    public void extractAllFilesToHostDirectory(File outputDir) throws IOException {
        final int ROOT_DIR_SECTORS = 14; // (224 entries * 32 bytes) / 512
        final int DATA_START_LBA = 33;   // settore dati dopo boot + 2 FAT + root dir

        int rootStartLBA = 1 + 9 * 2; // boot + 2 FAT da 9 settori

        for (int i = 0; i < ROOT_DIR_SECTORS; i++) {
            byte[] sector = getSector(rootStartLBA + i);
            for (int offset = 0; offset < 512; offset += 32) {
                byte firstByte = sector[offset];
                if (firstByte == 0x00 || (firstByte & 0xFF) == 0xE5) continue;

                int attrib = sector[offset + 11] & 0xFF;
                if ((attrib & 0x08) != 0 || (attrib & 0x10) != 0) continue; // skip label or directory

                String name = new String(sector, offset, 8).trim();
                String ext = new String(sector, offset + 8, 3).trim();
                String filename = name + (ext.isEmpty() ? "" : "." + ext);

                int startCluster = ((sector[offset + 27] & 0xFF) << 8) | (sector[offset + 26] & 0xFF);
                int fileSize =  ((sector[offset + 31] & 0xFF) << 24)
                        | ((sector[offset + 30] & 0xFF) << 16)
                        | ((sector[offset + 29] & 0xFF) << 8)
                        |  (sector[offset + 28] & 0xFF);

                if (startCluster < 2 || startCluster >= 0xFF0) continue;

                ByteArrayOutputStream fileData = new ByteArrayOutputStream();
                int cluster = startCluster;
                int remaining = fileSize;

                while (cluster < 0xFF8 && remaining > 0) {
                    int sectorIndex = DATA_START_LBA + (cluster - 2);
                    byte[] data = getSector(sectorIndex);

                    int toCopy = Math.min(512, remaining);
                    fileData.write(data, 0, toCopy);
                    remaining -= toCopy;

                    cluster = readFAT12Entry(cluster);
                }

                File outFile = new File(outputDir, filename);
                try (FileOutputStream fos = new FileOutputStream(outFile)) {
                    fos.write(fileData.toByteArray());
                }
            }
        }
    }

}

